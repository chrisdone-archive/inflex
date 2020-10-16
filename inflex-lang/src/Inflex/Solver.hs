{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
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
import           Data.Foldable
import           Data.Function
import qualified Data.Graph.Inductive.Graph as Graph
import qualified Data.Graph.Inductive.PatriciaTree as Graph
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Hashable
import           Data.List
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Map.Strict (Map)
import           Data.Maybe
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import           Data.Text (Text)
import           Debug.Trace
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

prune :: Seq EqualityConstraint -> (Seq EqualityConstraint, HashMap (TypeVariable Generated) (Type Generated))
prune cs =
  -- trace ("subs="++show subs)
  (Seq.fromList (fmap (substituteEqualityConstraint subs) (reverse keeps)), subs)
  where
    (subs,keeps) = foldl'
          (\(subs, keeps) q@EqualityConstraint {type1, type2} ->
             case (type1, type2) of
               (VariableType v1, VariableType v2)
                 | Set.member v1 cs' && Set.member v2 cs' ->
                   (subs, q:keeps)
                 | Set.member v1 cs' ->
                   (HM.insert v1 (VariableType v2) subs, keeps)
                 | Set.member v2 cs' ->
                   (HM.insert v2 (VariableType v1) subs, keeps)
               _ -> (subs, q:keeps))
          (mempty, mempty)
          cs
    cs' =
      Set.fromList . map (\(_, w) -> w) . Graph.labNodes . getTys . toGraph $ cs
    getTys ::
         Graph.Gr (TypeVariable Generated) EqualityConstraint
      -> Graph.Gr (TypeVariable Generated) EqualityConstraint
    getTys g = Graph.nfilter ((== 1) . Graph.deg g) g
    toGraph ::
         Seq EqualityConstraint
      -> Graph.Gr (TypeVariable Generated) EqualityConstraint
    toGraph sq =
      let r = Graph.mkGraph (Set.toList nodes) edges
       in -- trace (Graph.prettify r)
        r
      where
        list = toList sq
        nodes =
          Set.fromList
            (concatMap
               (\(tyvar1@TypeVariable {index = index1}, tyvar@TypeVariable {index}, _) ->
                  [(fromIntegral index1, tyvar1), (fromIntegral index, tyvar)])
               pairs)
        edges =
          map
            (\(TypeVariable {index = index1}, TypeVariable {index}, eq) ->
               ((fromIntegral index1), (fromIntegral index), eq))
            pairs
        pairs =
          concatMap
            (\eq@EqualityConstraint {type1, type2} ->
               [ (t1, t2, eq)
               | VariableType t1 <- [type1]
               , VariableType t2 <- [type2]
               ])
            list

solveText ::
     Map Hash (Either e (Scheme Polymorphic))
  -> FilePath
  -> Text
  -> Either (GenerateSolveError e) (IsSolved (Expression Solved))
solveText globals fp text = do
  -- trace ("what's going on here?" <> show text) (pure ())
  generated <- first GeneratorErrored (generateText globals fp text)
  solveGenerated generated

solveGenerated ::
     HasConstraints (Expression Generated)
  -> Either (GenerateSolveError e) (IsSolved (Expression Solved))
solveGenerated HasConstraints {thing = expression, mappings, equalities} =
  first
    SolverErrors
    (do -- trace "what up" (pure ())
        substitutions <- unifyConstraints subs equalities'
        -- trace (show substitutions) (pure ())

        pure
          IsSolved
            { thing = expressionSolve substitutions expression
            , mappings
            })
  where (equalities',subs) = prune equalities

unifyAndSubstitute ::
     Seq EqualityConstraint
  -> Type Generated
  -> Either (NonEmpty SolveError) (Type Solved)
unifyAndSubstitute equalities typ = do
  substitutions <- unifyConstraints subs equalities'
  pure (solveType substitutions (typ))
  where (equalities',subs) = prune equalities

--------------------------------------------------------------------------------
-- Unification

-- TODO: Change Seq Substitution to a @HashMap replaceme withthis@?

instance Hashable (TypeVariable Generated) where
  hashWithSalt s TypeVariable{index} = hashWithSalt s index

unifyConstraints ::
     HashMap (TypeVariable Generated) (Type Generated) -> Seq EqualityConstraint -> Either (NonEmpty SolveError) (HashMap (TypeVariable Generated) (Type Generated))
unifyConstraints =
  foldM
    (\(!existing) equalityConstraint ->
       fmap
         (\new -> extendSubstitutions Extension {existing, new})
         (unifyEqualityConstraint
            (substituteEqualityConstraint existing equalityConstraint)))


unifyEqualityConstraint :: EqualityConstraint -> Either (NonEmpty SolveError) (HashMap (TypeVariable Generated) (Type Generated))
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
    (ArrayType a, ArrayType b) ->
      unifyEqualityConstraint
        EqualityConstraint {location, type1 = a, type2 = b}
    _ -> Left (pure (TypeMismatch equalityConstraint))

unifyTypeApplications ::
     TypeApplication Generated
  -> TypeApplication Generated
  -> Either (NonEmpty SolveError) (HashMap (TypeVariable Generated) (Type Generated))
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
unifyRecords :: Type Generated -> Type Generated -> Either (NonEmpty SolveError) (HashMap (TypeVariable Generated) (Type Generated))
unifyRecords (RowType x) (RowType y) = unifyRows x y
unifyRecords _ _ = error "Invalid row types!" -- TODO: Make better error.

unifyRows ::
     TypeRow Generated
  -> TypeRow Generated
  -> Either (NonEmpty SolveError) (HashMap (TypeVariable Generated) (Type Generated))
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
      -- Below: If neither row has a variable, we have no row
      -- constraints to do. The fields will be unified below.
      (sd1, Nothing, sd2, Nothing)
        | sd1 `rowsExactMatch` sd2 -> do pure []
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
  unifyConstraints mempty (Seq.fromList
                    (fieldsToUnify <> constraintsToUnify))
  where
    rowsExactMatch = on (==) (Set.fromList . map fieldName)
    rowIsSubset = on Set.isSubsetOf (Set.fromList . map fieldName)
    fieldName Field {name} = name
    fieldType Field {typ} = typ

--------------------------------------------------------------------------------
-- Binding

bindTypeVariable :: TypeVariable Generated -> Type Generated -> Either (NonEmpty SolveError) (HashMap (TypeVariable Generated) (Type Generated))
bindTypeVariable typeVariable typ
  | typ == VariableType typeVariable = pure mempty
  | occursIn typeVariable typ = Left (pure (OccursCheckFail typeVariable typ))
  | typeVariableKind typeVariable /= typeKind typ = Left (pure (KindMismatch typeVariable typ))
  | otherwise = pure (HM.singleton typeVariable typ)

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
  { existing :: !(HashMap (TypeVariable Generated) (Type Generated))
  , new :: !(HashMap (TypeVariable Generated) (Type Generated))
  }

extendSubstitutions :: Extension -> HashMap (TypeVariable Generated) (Type Generated)
extendSubstitutions Extension {new, existing} = existing' <> new
  where
    existing' =
      fmap
        (substituteType new)
        existing

--------------------------------------------------------------------------------
-- Substitution

substituteEqualityConstraint ::
     HashMap (TypeVariable Generated) (Type Generated) -> EqualityConstraint -> EqualityConstraint
substituteEqualityConstraint substitutions equalityConstraint =
  EqualityConstraint
    { type1 = substituteType substitutions type1
    , type2 = substituteType substitutions type2
    , ..
    }
  where
    EqualityConstraint {type1, type2, ..} = equalityConstraint


-- tomap :: Seq Substitution -> HashMap (TypeVariable Generated) (Type Generated)
-- tomap = M.fromList . map (\(Substitution{before,after}) -> (before,after)) . toList

substituteType :: HashMap (TypeVariable Generated) (Type Generated) -> Type Generated -> Type Generated
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
          -- TODO: This is an O(n) operation. Bad in a type
          -- checker. May be the cause of slow down in array.
          case HM.lookup
                 typeVariable
                 substitutions of
            Just after ->  after -- added! -- substituteType substitutions

            Nothing -> typ
        RowType TypeRow {typeVariable = Just typeVariable, fields = xs, ..}
          | Just after <-
             HM.lookup
               typeVariable
               substitutions
          -- , substitutionKind substitution == RowKind
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

solveType :: HashMap (TypeVariable Generated) (Type Generated) -> Type Generated -> Type Solved
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

expressionSolve :: HashMap (TypeVariable Generated) (Type Generated) -> Expression Generated -> Expression Solved
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

lambdaSolve :: HashMap (TypeVariable Generated) (Type Generated) -> Lambda Generated -> Lambda Solved
lambdaSolve substitutions Lambda {..} =
  Lambda
    { param = paramSolve substitutions param
    , body = expressionSolve substitutions body
    , typ = solveType substitutions typ
    , ..
    }

propSolve :: HashMap (TypeVariable Generated) (Type Generated) -> Prop Generated -> Prop Solved
propSolve substitutions Prop {..} =
  Prop
    { expression = expressionSolve substitutions expression
    , typ = solveType substitutions typ
    , ..
    }

arraySolve :: HashMap (TypeVariable Generated) (Type Generated) -> Array Generated -> Array Solved
arraySolve substitutions Array {..} =
  Array
    { expressions = fmap (expressionSolve substitutions) expressions
    , typ = solveType substitutions typ
    , ..
    }

recordSolve :: HashMap (TypeVariable Generated) (Type Generated) -> Record Generated -> Record Solved
recordSolve substitutions Record {..} =
  Record
    { fields = map (fieldESolve substitutions) fields
    , typ = solveType substitutions typ
    , ..
    }

fieldESolve :: HashMap (TypeVariable Generated) (Type Generated) -> FieldE Generated -> FieldE Solved
fieldESolve substitutions FieldE {..} =
  FieldE
    { expression = expressionSolve substitutions expression
    , ..
    }

infixSolve :: HashMap (TypeVariable Generated) (Type Generated) -> Infix Generated -> Infix Solved
infixSolve substitutions Infix {..} =
  Infix
    { left = expressionSolve substitutions left
    , right = expressionSolve substitutions right
    , global = globalSolve substitutions global
    , typ = solveType substitutions typ
    , ..
    }

letSolve :: HashMap (TypeVariable Generated) (Type Generated) -> Let Generated -> Let Solved
letSolve substitutions Let {..} =
  Let
    { binds = fmap (bindSolve substitutions) binds
    , body = expressionSolve substitutions body
    , typ = solveType substitutions typ
    , ..
    }

bindSolve :: HashMap (TypeVariable Generated) (Type Generated) -> Bind Generated -> Bind Solved
bindSolve substitutions Bind {..} =
  Bind
    { param = paramSolve substitutions param
    , value = expressionSolve substitutions value
    , typ = solveType substitutions typ
    , ..
    }

applySolve :: HashMap (TypeVariable Generated) (Type Generated) -> Apply Generated -> Apply Solved
applySolve substitutions Apply {..} =
  Apply
    { function = expressionSolve substitutions function
    , argument = expressionSolve substitutions argument
    , typ = solveType substitutions typ
    , ..
    }

variableSolve :: HashMap (TypeVariable Generated) (Type Generated) -> Variable Generated -> Variable Solved
variableSolve substitutions Variable {..} =
  Variable {typ = solveType substitutions typ, ..}

globalSolve :: HashMap (TypeVariable Generated) (Type Generated) -> Global Generated -> Global Solved
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
        FunctionGlobal f -> FunctionGlobal f

solveScheme :: HashMap (TypeVariable Generated) (Type Generated) -> Scheme Generated -> Scheme Solved
solveScheme substitutions Scheme {..} =
  Scheme
    { typ = solveType substitutions typ
    , constraints = fmap (solveClassConstraint substitutions) constraints
    , ..
    }

solveClassConstraint :: HashMap (TypeVariable Generated) (Type Generated) -> ClassConstraint Generated -> ClassConstraint Solved
solveClassConstraint substitutions ClassConstraint {..} =
  ClassConstraint {typ = fmap (solveType substitutions) typ, ..}

literalSolve :: HashMap (TypeVariable Generated) (Type Generated) -> Literal Generated -> Literal Solved
literalSolve substitutions =
  \case
    TextLiteral LiteralText {..} ->
      TextLiteral LiteralText {typ = solveType substitutions typ, ..}
    NumberLiteral number -> NumberLiteral (numberSolve substitutions number)

numberSolve :: HashMap (TypeVariable Generated) (Type Generated) -> Number Generated -> Number Solved
numberSolve substitutions Number {..} =
  Number {typ = solveType substitutions typ, ..}

paramSolve :: HashMap (TypeVariable Generated) (Type Generated) -> Param Generated -> Param Solved
paramSolve substitutions Param {..} =
  Param {typ = solveType substitutions typ, ..}

substitutionKind :: Substitution -> Kind
substitutionKind Substitution {before} = typeVariableKind before
