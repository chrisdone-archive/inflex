{-# OPTIONS -F -pgmF=early #-}
{-# LANGUAGE TemplateHaskell #-}
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

module Inflex.Solver
  ( solveText
  , solveGenerated
  , unifyConstraints
  , unifyAndSubstitute
  , solveType
  , runSolver
  , solveTextRepl
  , freezeSubstitutions
  , Substitution(..)
  , SolveError(..)
  , IsSolved(..)
  , GenerateSolveError(..)
  , SolveReader(..)
  , SolveMsg(..)
  ) where

import           Control.DeepSeq
import           Control.Early (early, earlyThen)
import           Control.Monad.State.Strict
import           Data.Bifunctor
import           Data.Early
import           Data.Foldable
import           Data.Function
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.List
import           Data.Map.Strict (Map)
import           Data.Maybe
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import           Data.Void
import           Inflex.Generator
import           Inflex.Kind
import           Inflex.Types
import           Inflex.Types.Solver
import qualified RIO
import           RIO (RIO, glog)

--------------------------------------------------------------------------------
-- Top-level

solveText ::
     Map Hash (Either e (Scheme Polymorphic))
  -> FilePath
  -> Text
  -> RIO SolveReader (Either (GenerateSolveError e) (IsSolved (Expression Solved)))
solveText globals fp text = do
  generated <- pure (first GeneratorErrored (generateText globals fp text))
  case generated of
    Left e -> pure (Left e)
    Right generated' -> solveGenerated generated'

solveGenerated ::
     HasConstraints (Expression Generated)
  -> RIO SolveReader (Either (GenerateSolveError e) (IsSolved (Expression Solved)))
solveGenerated HasConstraints {thing = expression, mappings, equalities} =
  do
     fmap
       (first SolverError .
        second
          (\substitutions ->
             IsSolved {thing = expressionSolve substitutions expression, mappings}))
       (runSolver
          (do unifyConstraints equalities?
              fmap Right freezeSubstitutions))

runSolver :: Solve a -> RIO SolveReader a
runSolver = runSolve

unifyAndSubstitute ::
     Seq EqualityConstraint
  -> Type Generated
  -> Solve (Either SolveError (Type Solved))
unifyAndSubstitute equalities typ = do
  unifyConstraints equalities?
  substitutions <- freezeSubstitutions
  pure (Right (solveType substitutions typ))

solveTextRepl ::
     Text
  -> IO (Either (GenerateSolveError e) (IsSolved (Expression Solved)))
solveTextRepl text = do
  counterRef <- RIO.newSomeRef 0
  binds <- RIO.newSomeRef mempty
  RIO.runRIO
    (SolveReader
       { glogfunc = RIO.mkGLogFunc (\_cs msg -> print msg)
       , counter = counterRef
       , binds = binds
       })
    (solveText mempty "repl" text)

--------------------------------------------------------------------------------
-- Unification

unifyConstraints ::
     Seq EqualityConstraint -> Solve (Either SolveError ())
unifyConstraints constraints = do
  glog (UnifyConstraints (length constraints))
  traverseE_ unifyEqualityConstraint constraints

unifyEqualityConstraint ::
     EqualityConstraint
  -> Solve (Either SolveError ())
unifyEqualityConstraint equalityConstraint@EqualityConstraint { type1
                                                              , type2
                                                              , location
                                                              } = do
  glog (UnifyEqualityConstraint equalityConstraint)
  type1' <- expandSpine type1
  type2' <- expandSpine type2
  case (type1', type2') of
    (ApplyType typeApplication1, ApplyType typeApplication2) ->
      unifyTypeApplications typeApplication1 typeApplication2
    (VariableType typeVariable, typ) -> bindTypeVariable typeVariable typ
    (typ, VariableType typeVariable) -> bindTypeVariable typeVariable typ
    (ConstantType TypeConstant {name = typeConstant1}, ConstantType TypeConstant {name = typeConstant2})
      | typeConstant1 == typeConstant2 -> pure (Right mempty)
    (RowType x, RowType y) -> unifyRows x y
    (RecordType r1, RecordType r2) -> unifyRecords r1 r2
    (VariantType r1, VariantType r2) -> unifyRecords r1 r2
    (RecursiveType a, RecursiveType b) ->
      unifyEqualityConstraint
        EqualityConstraint {location, type1 = a, type2 = b}
    (ArrayType a, ArrayType b) ->
      unifyEqualityConstraint
        EqualityConstraint {location, type1 = a, type2 = b}
    (DeBruijnType i, DeBruijnType j)
      | i == j -> pure (Right mempty)
    _ -> pure (Left (TypeMismatch equalityConstraint))

unifyTypeApplications ::
     TypeApplication Generated
  -> TypeApplication Generated
  -> Solve (Either SolveError ())
unifyTypeApplications typeApplication1 typeApplication2 = do
  glog UnifyTypeApplications
  unifyEqualityConstraint
    EqualityConstraint {type1 = function1, type2 = function2, location}?
  unifyEqualityConstraint
    (EqualityConstraint {type1 = argument1, type2 = argument2, location})
  where
    TypeApplication {function = function1, argument = argument1, location} =
      typeApplication1
     -- TODO: set location properly. This will enable "provenance"
     -- <https://www.youtube.com/watch?v=rdVqQUOvxSU>
    TypeApplication {function = function2, argument = argument2} =
      typeApplication2

-- | Unify records -- must contain row types inside.
unifyRecords :: Type Generated -> Type Generated -> Solve (Either SolveError ())
unifyRecords (RowType x) (RowType y) = unifyRows x y
unifyRecords _ _ = pure (Left NotRowTypes)

unifyRows ::
     TypeRow Generated
  -> TypeRow Generated
  -> Solve (Either SolveError ())
unifyRows row1@(TypeRow {typeVariable = v1, fields = fs1, ..}) row2@(TypeRow { typeVariable = v2
                                                                             , fields = fs2
                                                                             }) = do
  glog (UnifyRows row1 row2)
  constraints <- generateConstraints?
  let !common = force $ intersect (map fieldName fs1) (map fieldName fs2)
      -- You have to make sure that the types of all the fields match
      -- up, obviously.
  let !fieldsToUnify =
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
  let !constraintsToUnify =
        map
          (\(tyvar, t) ->
             EqualityConstraint
               { type1 = VariableType tyvar
               , type2 = t
               , .. -- TODO: clever location.
               })
          constraints
  unifyConstraints (Seq.fromList (fieldsToUnify <> constraintsToUnify))
  where
    fieldName Field {name} = name
    fieldType Field {typ} = typ
    generateConstraints = do
      let --
          -- DISJOINT CALCULATION
          --
          -- This makes it easy to deal with two rows on each side
          -- that have disjoint fields, i.e. ones on the left aren't
          -- on the right and vise-versa.
          --
          -- Get all the fields from fs1 that are not in fs2.
          disjointFields1 =
            [ f1
            | f1@Field {name} <- fs1
            , name `notElem` map (\Field {name = name2} -> name2) fs2
            ]
          -- All the fields from fs2 that aren't in fs1.
          disjountFields2 =
            [ f1
            | f1@Field {name} <- fs2
            , name `notElem` map (\Field {name = name2} -> name2) fs1
            ]
      case (disjointFields1, v1, disjountFields2, v2) of
          -- Below: For empty fields, don't generate any constraints. Even for the type variables.
          ([], Just v1', [], Just v2')
            | v1' == v2' -> pure $ Right [] -- IF the variables are the same.
          ([], Nothing, [], Nothing) -> pure $ Right []
          --
          -- Below: Just unify a row variable with no fields with any other row.
          --
          ([], Just u, disjountFields, r) ->
            pure (Right [(,) u (RowType (TypeRow {typeVariable = r, fields = disjountFields, ..}))]) -- TODO: Merge locs, vars
          (disjountFields, r, [], Just u) ->
            pure (Right [(,) u (RowType (TypeRow {typeVariable = r, fields = disjountFields, ..}))]) -- TODO: Merge locs, vars
          --
          -- Below: Two open records, their fields must unify and we
          -- produce a union row type of both.
          --
          (_, Just u1, _, Just u2) -> do
            freshType <- generateTypeVariable' location RowUnifyPrefix RowKind
            let merged1 =
                  RowType
                    (TypeRow {typeVariable = Just freshType, fields = disjointFields1, ..})
                merged2 =
                  RowType
                    (TypeRow {typeVariable = Just freshType, fields = disjountFields2, ..})
            pure (Right [(u1, merged2), (u2, merged1)])
          --
          -- Below: If we got here, then the following is true:
          --
          -- 1) a. One side is an open row, and the other side is a closed row.
          --    b. Both sides are closed.
          --
          -- 2) Because we disjoin the two, so f([1,2],[1,2,3]) ->
          --    ([],[1,2,3]), we can expect an empty field set on one
          --    side, which is handled by the above cases.
          --
          -- 3) However, if don't have an empty side, then we arrive here.
          --
          --   a. If both sides are closed, that's an easy mismatch
          --      e.g. a closed function expects {x,y} and you give
          --      {k,q} then your fields just are wrong.
          --
          --   b. If one side is open, then that's either a field
          --      access or an open function, but in either case means
          --      that you aren't giving enough in the case of a
          --      function, or that your expectations are wrong in a
          --      property access.
          --
          --     It depends on whether we're in a function call or a
          --     property access.
          --
          _ -> pure (Left (RowMismatch row1 row2))

--------------------------------------------------------------------------------
-- Binding

bindTypeVariable ::
     TypeVariable Generated
  -> Type Generated
  -> Solve (Either SolveError ())
bindTypeVariable typeVariable typ
  | typ == VariableType typeVariable = pure (Right mempty)
  | occursIn typeVariable typ =
    pure (Left (OccursCheckFail typeVariable typ))
  | typeVariableKind typeVariable /= typeKind typ =
    pure (Left (KindMismatch typeVariable typ))
  | otherwise = do
    bindImperatively typeVariable typ
    glog (SuccessfulBindTypeVariable substitution)
    pure (Right ())
  where
    substitution = Substitution {before = typeVariable, after = typ}

bindImperatively :: TypeVariable Generated -> Type Generated -> Solve ()
bindImperatively (!typeVariable) (!typ) = do
  bindsRef <- RIO.asks (\SolveReader {binds} -> binds)
  !binds <- RIO.readSomeRef bindsRef
  case HM.lookup typeVariable binds of
    Nothing -> do
      ref <- RIO.newSomeRef typ
      RIO.modifySomeRef bindsRef (HM.insert typeVariable ref)
    Just ref -> RIO.writeSomeRef ref typ

expandSpine :: Type Generated -> Solve (Type Generated)
expandSpine ty = do
  case ty of
    VariableType tyvar -> do
      bindsRef <- RIO.asks (\SolveReader {binds} -> binds)
      bindsMap <- RIO.readSomeRef bindsRef
      case HM.lookup tyvar bindsMap of
        Nothing -> pure ty
        Just typ -> do
          ty' <- RIO.readSomeRef typ
          if ty' /= ty
            then expandSpine ty'
            else pure ty'
    _ -> pure ty

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
    RecursiveType typ -> occursIn typeVariable typ
    DeBruijnType {} -> False
    ArrayType x -> occursIn typeVariable x
    RowType TypeRow{typeVariable=mtypeVariable, fields} ->
      maybe False (occursIn typeVariable . VariableType) mtypeVariable ||
      any (\Field{typ} -> occursIn typeVariable typ) fields

--------------------------------------------------------------------------------
-- Substitution

substituteType :: HashMap (TypeVariable Generated) (Type Generated) -> Type Generated -> Type Generated
substituteType substitutions = go
  where
    go =
      \case
        FreshType v -> absurd v
        RecordType t -> RecordType (go t)
        VariantType t -> VariantType (go t)
        ArrayType t -> ArrayType (go t)
        RecursiveType typ -> RecursiveType (go typ)
        DeBruijnType i -> DeBruijnType i
        typ@ConstantType {} -> typ
        ApplyType TypeApplication {function, argument, ..} ->
          ApplyType
            TypeApplication {function = go function, argument = go argument, ..}
        typ@(VariableType typeVariable :: Type Generated) ->
          case HM.lookup typeVariable substitutions of
            Just after -> go after
            -- Above: Not sure why 'go' is necessary here, but adding
            -- it fixed an inferred type in the output meta data. I
            -- think it's fine, though.
            Nothing -> typ
        RowType TypeRow {typeVariable = Just typeVariable, fields = xs, ..}
          | Just after <- HM.lookup typeVariable substitutions
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
        FreshType v -> absurd v
        RecordType t -> RecordType (go t)
        VariantType t -> VariantType (go t)
        RecursiveType typ -> RecursiveType (go typ)
        DeBruijnType i -> DeBruijnType i
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
    FoldExpression fold' ->
      FoldExpression (foldSolve substitutions fold')
    UnfoldExpression unfold' ->
      UnfoldExpression (unfoldSolve substitutions unfold')
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
    IfExpression if' ->
      IfExpression (ifSolve substitutions if')
    CaseExpression case' ->
      CaseExpression (caseSolve substitutions case')
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

foldSolve :: HashMap (TypeVariable Generated) (Type Generated) -> Fold Generated -> Fold Solved
foldSolve substitutions Fold {..} =
  Fold
    { expression = expressionSolve substitutions expression
    , typ = solveType substitutions typ
    , ..
    }

unfoldSolve :: HashMap (TypeVariable Generated) (Type Generated) -> Unfold Generated -> Unfold Solved
unfoldSolve substitutions Unfold {..} =
  Unfold
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

variantSolve :: HashMap (TypeVariable Generated) (Type Generated) -> Variant Generated -> Variant Solved
variantSolve substitutions Variant {..} =
  Variant
    { argument = fmap (expressionSolve substitutions) argument
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

caseSolve :: HashMap (TypeVariable Generated) (Type Generated) -> Case Generated -> Case Solved
caseSolve substitutions Case {..} =
  Case
    { location
    , scrutinee = expressionSolve substitutions scrutinee
    , typ = solveType substitutions typ
    , alternatives =
        fmap
          (\Alternative {location = loc, ..} ->
             Alternative
               { pattern' =
                   case pattern' of
                     WildPattern hole ->
                       WildPattern (holeSolve substitutions hole)
                     ParamPattern param ->
                       ParamPattern (paramSolve substitutions param)
                     VariantPattern VariantP {location = locp, ..} ->
                       VariantPattern
                         VariantP
                           { location = locp
                           , tag
                           , argument = fmap (paramSolve substitutions) argument
                           }
               , expression = expressionSolve substitutions expression
               , location = loc
               , ..
               })
          alternatives
    }

ifSolve :: HashMap (TypeVariable Generated) (Type Generated) -> If Generated -> If Solved
ifSolve substitutions If {..} =
  If
    { condition = expressionSolve substitutions condition
    , consequent = expressionSolve substitutions consequent
    , alternative = expressionSolve substitutions alternative
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
        EqualGlobal e -> EqualGlobal e
        CompareGlobal e -> CompareGlobal e
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

holeSolve :: HashMap (TypeVariable Generated) (Type Generated) -> Hole Generated -> Hole Solved
holeSolve substitutions Hole {..} =
  Hole {typ = solveType substitutions typ, ..}

--------------------------------------------------------------------------------
-- Generate type variable

-- | Needed when unifying rows; we have to generate a fresh type at
-- that point.  The indexing is different to the generating stage, but
-- it doesn't matter, because the rest of the type variable's prefix
-- will differ.
generateTypeVariable' ::
     Cursor -> TypeVariablePrefix -> Kind -> Solve (TypeVariable Generated)
generateTypeVariable' location prefix kind = do
  index <- get
  glog (GeneratedTypeVariable prefix kind index)
  modify' succ
  pure
    TypeVariable {location, prefix = SolverGeneratedPrefix prefix, index, kind}

--------------------------------------------------------------------------------
-- Freeze substitutions

freezeSubstitutions :: Solve (HashMap (TypeVariable Generated) (Type Generated))
freezeSubstitutions = do
  bindsRef <- RIO.asks (\SolveReader {binds} -> binds)
  bindsMap <- RIO.readSomeRef bindsRef
  let go typ =
        case typ of
          FreshType v -> absurd v
          RecordType t -> fmap RecordType (go t)
          RecursiveType t -> fmap RecursiveType (go t)
          DeBruijnType i -> pure (DeBruijnType i)
          VariantType t -> fmap VariantType (go t)
          ArrayType t -> fmap ArrayType (go t)
          ConstantType {} -> pure typ
          VariableType typeVariable ->
            case HM.lookup typeVariable bindsMap of
              Nothing -> pure typ
              Just typ' -> RIO.readSomeRef typ' >>= go
          ApplyType TypeApplication {function, argument, ..} -> do
            function' <- go function
            argument' <- go argument
            pure
              (ApplyType
                 TypeApplication
                   {function = function', argument = argument', ..})
          RowType typeRow@TypeRow { typeVariable = Just typeTariable
                                  , fields = xs
                                  , ..
                                  }
            | Just after <- HM.lookup typeTariable bindsMap -> do
              typ' <- RIO.readSomeRef after
              case typ' of
                RowType TypeRow {typeVariable = newVariable, fields = ys} ->
                  pure
                    (RowType
                       TypeRow
                         { typeVariable = newVariable
                         , fields = shadowFields ys xs
                         , ..
                         })
                _ -> doFields typeRow
          RowType typeRow -> doFields typeRow
        where
          doFields TypeRow {..} = do
            fields' <- traverse substituteField fields
            pure (RowType TypeRow {fields = fields', ..})
            where
              substituteField Field {typ = typ0, location = l, ..} = do
                typ' <- go typ0
                pure Field {typ = typ', location = l, ..}
  traverse
    (\typeRef -> do
       typ <- RIO.readSomeRef typeRef >>= go
       pure typ)
    bindsMap
