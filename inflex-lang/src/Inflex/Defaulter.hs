{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Defaulting class instances that are ambiguous.

module Inflex.Defaulter
  ( defaultText
  , defaultResolvedExpression
  , DefaulterError(..)
  , DefaulterReader(..)
  , ResolverDefaulterError(..)
  ) where

import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Writer
import           Data.Bifunctor
import           Data.Foldable
import           Data.Functor.Identity
import qualified Data.List.NonEmpty as NE
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import           Data.Void
import           Inflex.Defaulter.Suggest
import           Inflex.Resolver
import           Inflex.Type
import           Inflex.Types
import           Inflex.Types.Defaulter
import           RIO

--------------------------------------------------------------------------------
-- Top-level entry points

data DefaulterReader = DefaulterReader

defaultText ::
     Map Hash (Either e (Scheme Polymorphic))
  -> FilePath
  -> Text
  -> RIO DefaulterReader (Either (ResolverDefaulterError e) Cell)
defaultText globals fp text = do
  resolved <-
    fmap
      (first GeneraliseResolverError)
      (runRIO ResolveReader (resolveText globals fp text))
  case resolved of
    Left err -> pure (Left err)
    Right resolved' ->
      fmap (first DefaulterError) (defaultResolvedExpression resolved')

defaultResolvedExpression ::
     IsResolved (Expression Resolved)
  -> RIO DefaulterReader (Either DefaulterError Cell)
defaultResolvedExpression IsResolved {scheme = scheme0, thing = expression} = pure (do
   classConstraintReplacements <- generateReplacements scheme0
   (scheme', defaults) <-
     runWriterT
       (foldM
          (\scheme@Scheme {constraints = acc, typ} classConstraint ->
             case M.lookup classConstraint classConstraintReplacements of
               Nothing -> pure scheme {constraints = acc ++ [classConstraint]}
               Just replacements -> do
                 let substitutions = Seq.fromList (toList replacements)
                     classConstraint' =
                       substituteClassConstraint substitutions classConstraint
                     typ' = substituteType substitutions typ
                 default' <-
                   lift (makeValidDefault classConstraint classConstraint')
                 tell (pure default')
                 pure (scheme {constraints = acc, typ = typ'}))
          scheme0 {constraints = mempty}
          originalConstraints)
   pure
     Cell
       { location
       , scheme = scheme'
       , defaultedClassConstraints = defaults
       , ambiguousClassConstraints = mempty -- TODO: Provide this info.
       , defaulted = applyDefaults originalConstraints defaults expression
       })
  where
    Scheme {constraints = originalConstraints, location} = scheme0

-- | Generate replacements for each class constraint that can be generated.
generateReplacements ::
     Scheme Polymorphic
  -> Either DefaulterError (Map (ClassConstraint Polymorphic) (Set Substitution))
generateReplacements scheme0 = do
  let typeVariableReplacements =
        M.mapMaybeWithKey
          (\_key constraints' ->
             runIdentity
               (fmap (fmap (constraints', )) (suggestTypeConstant constraints')))
          constrainedDefaultableTypeVariables
  let classConstraintReplacements =
        M.fromListWith
          (<>)
          (concatMap
             (\(typeVariable, (constraints', typ)) ->
                map
                  (\constraint ->
                     ( constraint
                     , Set.singleton
                         (Substitution {before = typeVariable, after = typ})))
                  (toList constraints'))
             (M.toList typeVariableReplacements))
  pure classConstraintReplacements
  where
    constrainedDefaultableTypeVariables =
      M.mapMaybe
        (NE.nonEmpty . toList)
        (M.intersectionWith
           (<>)
           (constraintedTypeVariables scheme0)
           (M.fromList
              (map (, mempty) (toList (defaultableTypeVariables scheme0)))))

--------------------------------------------------------------------------------
-- Applying defaults

-- | Traverse down the expression for each class constraint, and if
-- there is a default for that class constraint, apply a dictionary
-- argument to the lambda. If there's no default for that class
-- constraint, we step down into the lambda and continue.
applyDefaults ::
     [ClassConstraint Polymorphic]
  -> Seq (Default Polymorphic)
  -> Expression Resolved
  -> Expression Resolved
applyDefaults [] _ = id
applyDefaults (classConstraint:originalClassConstraints) defaults =
  \case
    LambdaExpression lambda@Lambda {body} ->
      immediatelyApplied
        (LambdaExpression
           (lambda {body = applyDefaults originalClassConstraints defaults body}))
    e -> error ("Unexpected expr: " ++ show e) -- TODO: Eliminate.
  where
    immediatelyApplied :: Expression Resolved -> Expression Resolved
    immediatelyApplied =
      case find
             (\Default {classConstraintOriginal} ->
                classConstraintOriginal == classConstraint)
             defaults of
        Nothing -> id
        Just Default {instanceName} ->
          \function ->
            ApplyExpression
              Apply
                { location = AutoInsertedForDefaulterCursor
                , typ = typeOutput (expressionType function)
                , function
                , argument =
                    GlobalExpression
                      Global
                        { location = AutoInsertedForDefaulterCursor
                        , scheme =
                            ResolvedScheme (instanceNameType instanceName)
                        , name = InstanceGlobal instanceName
                        }
                }

--------------------------------------------------------------------------------
-- Generating a default from a class constraint

-- Uses Inflex.Resolver.resolveConstraint to check that the suggested
-- types correctly produce an instance for the class constraint.
--
-- If it produces a ResolutionError, that's a hard fail. If no instance
-- is found, that's a hard fail.

-- | We check to see whether the defaulted class constraint is valid.
makeValidDefault ::
     ClassConstraint Polymorphic
  -> ClassConstraint Polymorphic
  -> Either DefaulterError (Default Polymorphic)
makeValidDefault classConstraintOriginal classConstraintDefaulted = do
  resolutionSuccess <-
    first ResolutionError (resolvePolyConstraint classConstraintDefaulted)
  case resolutionSuccess of
    InstanceFound instanceName ->
      pure
        Default
          {classConstraintOriginal, classConstraintDefaulted, instanceName}
    NoInstanceButPoly noInstanceConstraint ->
      Left (DefaultingNoInstanceFound noInstanceConstraint)

--------------------------------------------------------------------------------
-- Type variables mentioned in the class constraints

-- | Access type variables via @constraintsTypeVariables@.
constraintedTypeVariables ::
     Scheme Polymorphic
  -> Map (TypeVariable Polymorphic) (Set (ClassConstraint Polymorphic))
constraintedTypeVariables Scheme {constraints} =
  constraintsTypeVariablesPolymorphic constraints

--------------------------------------------------------------------------------
-- Find type variables which can be defaulted

-- | Produce the unique set of variables that may meaningfully be
-- defaulted for a cell.
--
-- I'm 90% sure that this is the right way to attack the problem of
-- defaulting cells. But I don't know of a precedent in the
-- literature/implementation world.
--
-- Examples:
--
-- (FromInteger a, FromDecimal b) => (a -> b) => {}
-- (FromInteger a, FromDecimal b) => {x: a, y: b} => {a,b}
-- (FromInteger a, FromDecimal b, FromDecimal c) => {x: a, y: c -> b} => {a}
-- (FromDecimal b, FromDecimal c) => {x: c, y: c -> b} => {c}
--
defaultableTypeVariables :: Scheme Polymorphic -> Set (TypeVariable Polymorphic)
defaultableTypeVariables Scheme {typ} = typeVariables typ
  where
    typeVariables =
      \case
        FreshType v -> absurd v
        RecordType t -> typeVariables t
        VariantType t -> typeVariables t
        ArrayType t -> typeVariables t
        VariableType typeVariable -> Set.singleton typeVariable
        ApplyType TypeApplication {function, argument} ->
          case function of
            ConstantType TypeConstant {name = FunctionTypeName} ->
              mempty -- We ignore the whole function.
            _ -> typeVariables function <> typeVariables argument
        ConstantType {} -> mempty
        -- Below: we don't default row types.
        RowType TypeRow {typeVariable = _, fields} ->
          foldMap (\Field {typ=t} -> typeVariables t) fields

--------------------------------------------------------------------------------
-- Substitution

data Substitution = Substitution
  { before :: !(TypeVariable Polymorphic)
  , after :: !(Type Polymorphic)
  } deriving (Show, Eq, Ord)

substituteClassConstraint ::
     Seq Substitution
  -> ClassConstraint Polymorphic
  -> ClassConstraint Polymorphic
substituteClassConstraint substitutions ClassConstraint {..} =
  ClassConstraint {typ = fmap (substituteType substitutions) typ, ..}

substituteType :: Seq Substitution -> Type Polymorphic -> Type Polymorphic
substituteType substitutions = go
  where
    go =
      \case
        FreshType v -> absurd v
        RecordType t -> RecordType (go t)
        VariantType t -> VariantType (go t)
        ArrayType t -> ArrayType (go t)
        typ@ConstantType {} -> typ
        ApplyType TypeApplication {function, argument, ..} ->
          ApplyType
            TypeApplication {function = go function, argument = go argument, ..}
        RowType TypeRow {..} ->
          RowType TypeRow {fields = map fieldSub fields, ..}
        typ@(VariableType typeVariable :: Type Polymorphic) ->
          case find
                 (\Substitution {before} -> before == typeVariable)
                 substitutions of
            Just Substitution {after} -> after
            Nothing -> typ
    fieldSub Field {..} = Field {typ = substituteType substitutions typ, ..}
