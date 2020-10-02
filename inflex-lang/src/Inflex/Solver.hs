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

module Inflex.Solver where

import           Control.Monad
import           Data.Bifunctor
import           Data.Function
import           Data.List
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Map.Strict (Map)
import           Data.Maybe
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import           Inflex.Generator
import           Inflex.Kind
import           Inflex.Types

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
    (do substitutions <- unifyConstraints equalities
        pure
          IsSolved
            { thing = expressionSolve substitutions expression
            , mappings
            })

unifyAndSubstitute ::
     Seq EqualityConstraint
  -> Type Generated
  -> Either (NonEmpty SolveError) (Type Solved)
unifyAndSubstitute equalities typ = do
  substitutions <- unifyConstraints equalities
  pure (solveType substitutions typ)

--------------------------------------------------------------------------------
-- Unification

unifyConstraints ::
     Seq EqualityConstraint -> Either (NonEmpty SolveError) (Seq Substitution)
unifyConstraints =
  foldM
    (\existing equalityConstraint ->
       fmap
         (\new -> extendSubstitutions Extension {existing, new})
         (unifyEqualityConstraint
            (substituteEqualityConstraint existing equalityConstraint)))
    mempty

unifyEqualityConstraint :: EqualityConstraint -> Either (NonEmpty SolveError) (Seq Substitution)
unifyEqualityConstraint equalityConstraint@EqualityConstraint {type1, type2} =
  case (type1, type2) of
    (ApplyType typeApplication1, ApplyType typeApplication2) ->
      unifyTypeApplications typeApplication1 typeApplication2
    (VariableType typeVariable, typ) -> bindTypeVariable typeVariable typ
    (typ, VariableType typeVariable) -> bindTypeVariable typeVariable typ
    (ConstantType TypeConstant {name = typeConstant1}, ConstantType TypeConstant {name = typeConstant2})
      | typeConstant1 == typeConstant2 -> pure mempty
    (RowType x, RowType y) -> unifyRows x y
    (RecordType r1, RecordType r2) -> unifyRecords r1 r2
    _ -> Left (pure (TypeMismatch equalityConstraint))

unifyTypeApplications ::
     TypeApplication Generated
  -> TypeApplication Generated
  -> Either (NonEmpty SolveError) (Seq Substitution)
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
unifyRecords :: Type Generated -> Type Generated -> Either (NonEmpty SolveError) (Seq Substitution)
unifyRecords (RowType x) (RowType y) = unifyRows x y
unifyRecords _ _ = error "Invalid row types!" -- TODO: Make better error.

unifyRows ::
     TypeRow Generated
  -> TypeRow Generated
  -> Either (NonEmpty SolveError) (Seq Substitution)
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
      (_, Just {}, _, Just {}) -> do
        pure []
      _ -> Left (pure (RowMismatch row1 row2))
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
    rowIsSubset sub sup = all (`elem` map fieldName sup) (map fieldName sub)
    fieldName Field {name} = name
    fieldType Field {typ} = typ

--------------------------------------------------------------------------------
-- Binding

bindTypeVariable :: TypeVariable Generated -> Type Generated -> Either (NonEmpty SolveError) (Seq Substitution)
bindTypeVariable typeVariable typ
  | typ == VariableType typeVariable = pure mempty
  | occursIn typeVariable typ = Left (pure (OccursCheckFail typeVariable typ))
  | typeVariableKind typeVariable /= typeKind typ = Left (pure (KindMismatch typeVariable typ))
  | otherwise = pure (pure Substitution {before = typeVariable, after = typ})

occursIn :: TypeVariable Generated -> Type Generated -> Bool
occursIn typeVariable =
  \case
    VariableType typeVariable' -> typeVariable == typeVariable'
    ApplyType TypeApplication {function, argument} ->
      occursIn typeVariable function || occursIn typeVariable argument
    ConstantType {} -> False
    RecordType x -> occursIn typeVariable x
    ArrayType x -> occursIn typeVariable x
    RowType TypeRow{typeVariable=mtypeVariable, fields} ->
      maybe False (occursIn typeVariable . VariableType) mtypeVariable ||
      any (\Field{typ} -> occursIn typeVariable typ) fields

--------------------------------------------------------------------------------
-- Extension

data Extension = Extension
  { existing :: Seq Substitution
  , new :: Seq Substitution
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
        RecordType t -> RecordType (go t)
        ArrayType t -> ArrayType (go t)
        typ@ConstantType {} -> typ
        ApplyType TypeApplication {function, argument, ..} ->
          ApplyType
            TypeApplication {function = go function, argument = go argument, ..}
        typ@(VariableType typeVariable :: Type Generated) ->
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
        RecordType t -> RecordType (go t)
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
    ArrayExpression array ->
      ArrayExpression (arraySolve substitutions array)
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
        FromIntegerGlobal -> FromIntegerGlobal
        FromDecimalGlobal -> FromDecimalGlobal
        NumericBinOpGlobal n -> NumericBinOpGlobal n

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
    NumberLiteral number ->
      NumberLiteral (numberSolve substitutions number)

numberSolve :: Seq Substitution -> Number Generated -> Number Solved
numberSolve substitutions Number {..} =
  Number {typ = solveType substitutions typ, ..}

paramSolve :: Seq Substitution -> Param Generated -> Param Solved
paramSolve substitutions Param {..} =
  Param {typ = solveType substitutions typ, ..}

substitutionKind :: Substitution -> Kind
substitutionKind Substitution {before} = typeVariableKind before
