{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

-- | Solve equality constraints, updating all type variables in the AST.

module Inflex.Solver
  ( solveText
  , solveGenerated
  , unifyConstraints
  , unifyAndSubstitute
  , solveType
  , runSolver
  , Substitution(..)
  , SolveError(..)
  , IsSolved(..)
  , GenerateSolveError(..)
  ) where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Data.Bifunctor
import           Data.Function
import           Data.List
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Map.Strict (Map)
import           Data.Maybe
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import           Data.Text (Text)
import           Data.Void
import           Inflex.Generator
import           Inflex.Kind
import           Inflex.Types
import           Numeric.Natural

--------------------------------------------------------------------------------
-- Solver types

data SolveError
  = OccursCheckFail (TypeVariable Generated) (Type Generated)
  | KindMismatch (TypeVariable Generated) (Type Generated)
  | TypeMismatch EqualityConstraint
  | RowMismatch (TypeRow Generated) (TypeRow Generated)
  deriving (Show, Eq)

data GenerateSolveError e
  = SolverErrors (NonEmpty SolveError)
  | GeneratorErrored (RenameGenerateError e)
  deriving (Show, Eq)

data IsSolved a = IsSolved
  { thing :: !a
  , mappings :: !(Map Cursor SourceLocation)
  } deriving (Show, Eq)

data Substitution = Substitution
  { before :: !(TypeVariable Generated)
  , after :: !(Type Generated)
  } deriving (Show, Eq)

newtype Solve a = Solve
  { runSolve :: StateT Natural (Either (NonEmpty SolveError)) a
  } deriving ( MonadState Natural
             , Monad
             , Functor
             , Applicative
             , MonadError (NonEmpty SolveError)
             )

--------------------------------------------------------------------------------
-- Top-level

solveText ::
     Map Hash (Either e (Scheme Polymorphic))
  -> FilePath
  -> Text
  -> Either (GenerateSolveError e) (IsSolved (Expression Solved))
solveText globals fp text = do
  generated <- first GeneratorErrored (generateText globals fp text)
  solveGenerated generated

solveGenerated ::
     HasConstraints (Expression Generated)
  -> Either (GenerateSolveError e) (IsSolved (Expression Solved))
solveGenerated HasConstraints {thing = expression, mappings, equalities} =
  first
    SolverErrors
    (do substitutions <- runSolver (unifyConstraints equalities)
        pure
          IsSolved
            { thing = expressionSolve substitutions expression
            , mappings
            })

runSolver :: Solve a -> Either (NonEmpty SolveError) a
runSolver = flip evalStateT 0 . runSolve

unifyAndSubstitute ::
     Seq EqualityConstraint
  -> Type Generated
  -> Solve (Type Solved)
unifyAndSubstitute equalities typ = do
  substitutions <- unifyConstraints equalities
  pure (solveType substitutions typ)

--------------------------------------------------------------------------------
-- Unification

-- TODO: Change Seq Substitution to a @Map replaceme withthis@?
-- Doesn't save much time presently, but may in future. The current
-- largest hit is in unifyConstraints with O(n^2) behavior.

unifyConstraints ::
     Seq EqualityConstraint -> Solve (Seq Substitution)
unifyConstraints =
  -- This is a large speed hit. If there are 1000 elements in an
  -- array, it will iterate at least 1000x times. So it performs
  -- O(n). Meanwhile, the length of the 'existing' increases over time
  -- too. And extendSubstitutions performs an O(n) map over the
  -- existing set of subs.
  --
  -- That adds up to O(n^2) time, which is (of course) very poorly
  -- performing. Consider alternative ways to express this function
  -- without paying this penalty.
  foldM
    (\existing equalityConstraint ->
       fmap
         (\new -> extendSubstitutions Extension {existing, new})
         (unifyEqualityConstraint
            (substituteEqualityConstraint existing equalityConstraint)))
    mempty

unifyEqualityConstraint :: EqualityConstraint -> Solve (Seq Substitution)
unifyEqualityConstraint equalityConstraint@EqualityConstraint { type1
                                                              , type2
                                                              , location
                                                              } =
  case (type1, type2) of
    (ApplyType typeApplication1, ApplyType typeApplication2) ->
      unifyTypeApplications typeApplication1 typeApplication2
    (VariableType typeVariable, typ) -> bindTypeVariable typeVariable typ
    (typ, VariableType typeVariable) -> bindTypeVariable typeVariable typ
    (ConstantType TypeConstant {name = typeConstant1}, ConstantType TypeConstant {name = typeConstant2})
      | typeConstant1 == typeConstant2 -> pure mempty
    (RowType x, RowType y) -> unifyRows x y
    (RecordType r1, RecordType r2) -> unifyRecords r1 r2
    (VariantType r1, VariantType r2) -> unifyRecords r1 r2
    (ArrayType a, ArrayType b) ->
      unifyEqualityConstraint
        EqualityConstraint {location, type1 = a, type2 = b}
    _ -> throwError (pure (TypeMismatch equalityConstraint))

unifyTypeApplications ::
     TypeApplication Generated
  -> TypeApplication Generated
  -> Solve (Seq Substitution)
unifyTypeApplications typeApplication1 typeApplication2 = do
  existing <-
    unifyEqualityConstraint
      EqualityConstraint {type1 = function1, type2 = function2, location}
  new <-
    unifyEqualityConstraint
      (substituteEqualityConstraint
         existing
         (EqualityConstraint {type1 = argument1, type2 = argument2, location}))
  pure (extendSubstitutions Extension {existing, new})
  where
    TypeApplication {function = function1, argument = argument1, location}
     = typeApplication1
     -- TODO: set location properly. This will enable "provenance"
     -- <https://www.youtube.com/watch?v=rdVqQUOvxSU>
    TypeApplication {function = function2, argument = argument2} =
      typeApplication2

-- | Unify records -- must contain row types inside.
unifyRecords :: Type Generated -> Type Generated -> Solve (Seq Substitution)
unifyRecords (RowType x) (RowType y) = unifyRows x y
unifyRecords _ _ = error "Invalid row types!" -- TODO: Make better error.

unifyRows ::
     TypeRow Generated
  -> TypeRow Generated
  -> Solve (Seq Substitution)
unifyRows row1@(TypeRow {typeVariable = v1, fields = fs1, ..}) row2@(TypeRow { typeVariable = v2
                                                                             , fields = fs2
                                                                             }) = do
  constraints <-
    case (fs1, v1, fs2, v2) of
      ([], Nothing, [], Nothing) -> pure []
      -- Below: Just unify a row variable with no fields with any other row.
      ([], Just u, sd, r) ->
        pure [(,) u (RowType (TypeRow {typeVariable = r, fields = sd, ..}))] -- TODO: Merge locs, vars
      (sd, r, [], Just u) ->
        pure [(,) u (RowType (TypeRow {typeVariable = r, fields = sd, ..}))] -- TODO: Merge locs, vars
      -- Below: A closed row { f: t, p: s } unifies with an open row { p: s | r },
      -- forming { p: s, f: t }, provided the open row is a subset
      -- of the closed row.
      (sd1, Nothing, sd2, Just u2)
        | sd2 `rowIsSubset` sd1 -> do
          pure
            [ (,)
                u2
                (RowType
                   (TypeRow
                      { typeVariable = Nothing
                      , fields = (nubBy (on (==) fieldName) (sd1 <> sd2))
                      , .. -- TODO: Merge locs, vars
                      }))
            ]
       -- Below: Same as above, but flipped.
      (sd1, Just u1, sd2, Nothing)
        | sd1 `rowIsSubset` sd2 -> do
          pure
            [ (,)
                u1
                (RowType
                   TypeRow
                     { typeVariable = Nothing
                     , fields = (nubBy (on (==) fieldName) (sd1 <> sd2))
                     , .. -- TODO: Merge locs, vars
                     })
            ]
      -- Below: Two open records, their fields must unify and we
      -- produce a union row type of both.
      (sd1, Just u1, sd2, Just u2) -> do
        freshType <-
          generateTypeVariable location RowUnifyPrefix RowKind
        let mergedRowType =
              RowType
                (TypeRow
                   {typeVariable = Just freshType, fields = shadowFields sd2 sd1, ..})
        pure [(u2, mergedRowType), (u1, mergedRowType)]
      -- Below: If neither row has a variable, we have no row
      -- constraints to do. The fields will be unified below.
      (sd1, Nothing, sd2, Nothing)
        | sd1 `rowsExactMatch` sd2 -> do pure []
      _ -> throwError (pure (RowMismatch row1 row2))
  let common = intersect (map fieldName fs1) (map fieldName fs2)
      -- You have to make sure that the types of all the fields match
      -- up, obviously.
      fieldsToUnify =
        mapMaybe
          (\name -> do
             f1 <- find ((== name) . fieldName) fs1
             f2 <- find ((== name) . fieldName) fs2
             pure
               EqualityConstraint
                 { type1 = fieldType f1
                 , type2 = fieldType f2
                 , .. -- TODO: clever location.
                 })
          common
      -- These are essentially substitutions -- replacing one of the
      -- rows with something else.
      constraintsToUnify =
        map
          (\(tyvar, t) ->
             EqualityConstraint
               { type1 = VariableType tyvar
               , type2 = t
               , .. -- TODO: clever location.
               })
          constraints
  -- TODO: confirm that unifyConstraints is the right function to use.
  unifyConstraints (Seq.fromList (fieldsToUnify <> constraintsToUnify))
  where
    rowsExactMatch = on (==) (Set.fromList . map fieldName)
    rowIsSubset = on Set.isSubsetOf (Set.fromList . map fieldName)
    fieldName Field {name} = name
    fieldType Field {typ} = typ

--------------------------------------------------------------------------------
-- Binding

bindTypeVariable :: TypeVariable Generated -> Type Generated -> Solve (Seq Substitution)
bindTypeVariable typeVariable typ
  | typ == VariableType typeVariable = pure mempty
  | occursIn typeVariable typ = throwError (pure (OccursCheckFail typeVariable typ))
  | typeVariableKind typeVariable /= typeKind typ = throwError (pure (KindMismatch typeVariable typ))
  | otherwise = pure (pure Substitution {before = typeVariable, after = typ})

occursIn :: TypeVariable Generated -> Type Generated -> Bool
occursIn typeVariable =
  \case
    FreshType v -> absurd v
    VariableType typeVariable' -> typeVariable == typeVariable'
    ApplyType TypeApplication {function, argument} ->
      occursIn typeVariable function || occursIn typeVariable argument
    ConstantType {} -> False
    RecordType x -> occursIn typeVariable x
    VariantType x -> occursIn typeVariable x
    ArrayType x -> occursIn typeVariable x
    RowType TypeRow{typeVariable=mtypeVariable, fields} ->
      maybe False (occursIn typeVariable . VariableType) mtypeVariable ||
      any (\Field{typ} -> occursIn typeVariable typ) fields

--------------------------------------------------------------------------------
-- Extension

data Extension = Extension
  { existing :: !(Seq Substitution)
  , new :: !(Seq Substitution)
  }

extendSubstitutions :: Extension -> Seq Substitution
extendSubstitutions Extension {new, existing} = existing' <> new
  where
    existing' =
      fmap
        (\Substitution {after, ..} ->
           Substitution {after = substituteType new after, ..})
        existing

--------------------------------------------------------------------------------
-- Substitution

substituteEqualityConstraint ::
     Seq Substitution -> EqualityConstraint -> EqualityConstraint
substituteEqualityConstraint substitutions equalityConstraint =
  EqualityConstraint
    { type1 = substituteType substitutions type1
    , type2 = substituteType substitutions type2
    , ..
    }
  where
    EqualityConstraint {type1, type2, ..} = equalityConstraint

substituteType :: Seq Substitution -> Type Generated -> Type Generated
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
        typ@(VariableType typeVariable :: Type Generated) ->
          -- TODO: This is an O(n) operation. Bad in a type
          -- checker. May be the cause of slow down in array.
          case find
                 (\Substitution {before} -> before == typeVariable)
                 substitutions of
            Just substitution@Substitution {after}
              | substitutionKind substitution == typeVariableKind typeVariable ->
                after
              | otherwise -> typ -- TODO: error/signal problem.
            Nothing -> typ
        RowType TypeRow {typeVariable = Just typeVariable, fields = xs, ..}
          | Just substitution@Substitution {after} <-
             -- TODO: This is an O(n) operation. Bad in a type
             -- checker.
             find
               (\Substitution {before} -> before == typeVariable)
               substitutions
          , substitutionKind substitution == RowKind
          , RowType TypeRow {typeVariable = newVariable, fields = ys} <- after ->
            RowType -- Here we merge the two field sets with shadowing.
              (TypeRow
                 { typeVariable = newVariable
                 , fields = shadowFields ys xs
                 , .. -- TODO: Merge locations? And type variable locations?
                 })
        -- The row variables differ, so we can just substitute within the fields.
        RowType TypeRow {..} ->
          RowType TypeRow {fields = map substituteField fields, ..}
    substituteField Field {..} =
      Field {typ = substituteType substitutions typ, ..}

-- | Extend a record, shadowing existing fields.
shadowFields ::
     [Field Generated] -- ^ New fields
  -> [Field Generated] -- ^ Old fields
  -> [Field Generated] -- ^ Union of the two rows
shadowFields = unionBy (on (==) (\Field{name} -> name))

--------------------------------------------------------------------------------
-- Solving (i.e. substitution, but we also change the type from
-- Generated to Solved)

solveType :: Seq Substitution -> Type Generated -> Type Solved
solveType substitutions = go . substituteType substitutions
  where
    go =
      \case
        FreshType v -> absurd v
        RecordType t -> RecordType (go t)
        VariantType t -> VariantType (go t)
        ArrayType t -> ArrayType (go t)
        VariableType TypeVariable {..} -> VariableType TypeVariable {..}
        ApplyType TypeApplication {function, argument, ..} ->
          ApplyType
            TypeApplication {function = go function, argument = go argument, ..}
        ConstantType TypeConstant {..} -> ConstantType TypeConstant {..}
        RowType TypeRow {..} ->
          RowType
            TypeRow
              { fields = fmap fieldSolve fields
              , typeVariable = fmap typeVarSolve typeVariable
              , ..
              }
    fieldSolve Field {..} = Field {typ = solveType substitutions typ, ..}
    typeVarSolve TypeVariable {..} = TypeVariable {..}

expressionSolve :: Seq Substitution -> Expression Generated -> Expression Solved
expressionSolve substitutions =
  \case
    LiteralExpression literal ->
      LiteralExpression (literalSolve substitutions literal)
    PropExpression prop ->
      PropExpression (propSolve substitutions prop)
    HoleExpression hole ->
      HoleExpression (holeSolve substitutions hole)
    ArrayExpression array ->
      ArrayExpression (arraySolve substitutions array)
    VariantExpression variant ->
      VariantExpression (variantSolve substitutions variant)
    RecordExpression record ->
      RecordExpression (recordSolve substitutions record)
    LambdaExpression lambda ->
      LambdaExpression (lambdaSolve substitutions lambda)
    LetExpression let' ->
      LetExpression (letSolve substitutions let')
    InfixExpression infix' ->
      InfixExpression (infixSolve substitutions infix')
    ApplyExpression apply ->
      ApplyExpression (applySolve substitutions apply)
    VariableExpression variable ->
      VariableExpression (variableSolve substitutions variable)
    GlobalExpression global ->
      GlobalExpression (globalSolve substitutions global)

lambdaSolve :: Seq Substitution -> Lambda Generated -> Lambda Solved
lambdaSolve substitutions Lambda {..} =
  Lambda
    { param = paramSolve substitutions param
    , body = expressionSolve substitutions body
    , typ = solveType substitutions typ
    , ..
    }

propSolve :: Seq Substitution -> Prop Generated -> Prop Solved
propSolve substitutions Prop {..} =
  Prop
    { expression = expressionSolve substitutions expression
    , typ = solveType substitutions typ
    , ..
    }

arraySolve :: Seq Substitution -> Array Generated -> Array Solved
arraySolve substitutions Array {..} =
  Array
    { expressions = fmap (expressionSolve substitutions) expressions
    , typ = solveType substitutions typ
    , ..
    }

variantSolve :: Seq Substitution -> Variant Generated -> Variant Solved
variantSolve substitutions Variant {..} =
  Variant
    { argument = fmap (expressionSolve substitutions) argument
    , typ = solveType substitutions typ
    , ..
    }

recordSolve :: Seq Substitution -> Record Generated -> Record Solved
recordSolve substitutions Record {..} =
  Record
    { fields = map (fieldESolve substitutions) fields
    , typ = solveType substitutions typ
    , ..
    }

fieldESolve :: Seq Substitution -> FieldE Generated -> FieldE Solved
fieldESolve substitutions FieldE {..} =
  FieldE
    { expression = expressionSolve substitutions expression
    , ..
    }

infixSolve :: Seq Substitution -> Infix Generated -> Infix Solved
infixSolve substitutions Infix {..} =
  Infix
    { left = expressionSolve substitutions left
    , right = expressionSolve substitutions right
    , global = globalSolve substitutions global
    , typ = solveType substitutions typ
    , ..
    }

letSolve :: Seq Substitution -> Let Generated -> Let Solved
letSolve substitutions Let {..} =
  Let
    { binds = fmap (bindSolve substitutions) binds
    , body = expressionSolve substitutions body
    , typ = solveType substitutions typ
    , ..
    }

bindSolve :: Seq Substitution -> Bind Generated -> Bind Solved
bindSolve substitutions Bind {..} =
  Bind
    { param = paramSolve substitutions param
    , value = expressionSolve substitutions value
    , typ = solveType substitutions typ
    , ..
    }

applySolve :: Seq Substitution -> Apply Generated -> Apply Solved
applySolve substitutions Apply {..} =
  Apply
    { function = expressionSolve substitutions function
    , argument = expressionSolve substitutions argument
    , typ = solveType substitutions typ
    , ..
    }

variableSolve :: Seq Substitution -> Variable Generated -> Variable Solved
variableSolve substitutions Variable {..} =
  Variable {typ = solveType substitutions typ, ..}

globalSolve :: Seq Substitution -> Global Generated -> Global Solved
globalSolve substitutions Global {scheme = GeneratedScheme scheme, ..} =
  Global
    {scheme = SolvedScheme (solveScheme substitutions scheme), name = refl, ..}
  where
    refl =
      case name of
        HashGlobal x -> HashGlobal x
        EqualGlobal e -> EqualGlobal e
        FromIntegerGlobal -> FromIntegerGlobal
        FromDecimalGlobal -> FromDecimalGlobal
        NumericBinOpGlobal n -> NumericBinOpGlobal n
        FunctionGlobal f -> FunctionGlobal f

solveScheme :: Seq Substitution -> Scheme Generated -> Scheme Solved
solveScheme substitutions Scheme {..} =
  Scheme
    { typ = solveType substitutions typ
    , constraints = fmap (solveClassConstraint substitutions) constraints
    , ..
    }

solveClassConstraint :: Seq Substitution -> ClassConstraint Generated -> ClassConstraint Solved
solveClassConstraint substitutions ClassConstraint {..} =
  ClassConstraint {typ = fmap (solveType substitutions) typ, ..}

literalSolve :: Seq Substitution -> Literal Generated -> Literal Solved
literalSolve substitutions =
  \case
    TextLiteral LiteralText {..} ->
      TextLiteral LiteralText {typ = solveType substitutions typ, ..}
    NumberLiteral number -> NumberLiteral (numberSolve substitutions number)

numberSolve :: Seq Substitution -> Number Generated -> Number Solved
numberSolve substitutions Number {..} =
  Number {typ = solveType substitutions typ, ..}

paramSolve :: Seq Substitution -> Param Generated -> Param Solved
paramSolve substitutions Param {..} =
  Param {typ = solveType substitutions typ, ..}

holeSolve :: Seq Substitution -> Hole Generated -> Hole Solved
holeSolve substitutions Hole {..} =
  Hole {typ = solveType substitutions typ, ..}

substitutionKind :: Substitution -> Kind
substitutionKind Substitution {before} = typeVariableKind before

--------------------------------------------------------------------------------
-- Generate type variable

-- | Needed when unifying rows; we have to generate a fresh type at
-- that point.  The indexing is different to the generating stage, but
-- it doesn't matter, because the rest of the type variable's prefix
-- will differ.
generateTypeVariable ::
     Cursor -> TypeVariablePrefix -> Kind -> Solve (TypeVariable Generated)
generateTypeVariable location prefix kind = do
  index <- get
  modify succ
  pure
    TypeVariable {location, prefix = SolverGeneratedPrefix prefix, index, kind}
