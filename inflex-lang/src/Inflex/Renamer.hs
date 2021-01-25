{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Renamer for Inflex language.

module Inflex.Renamer
  ( renameText
  , IsRenamed(..)
  , RenameError(..)
  , ParseRenameError(..)
  , patternParam
  ) where

import           Control.Monad.State
import           Control.Monad.Validate
import           Data.Bifunctor
import           Data.Foldable
import           Data.List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Vector as V
import           Inflex.Instances ()
import           Inflex.Parser
import           Inflex.Type
import           Inflex.Types
import           Inflex.Types.Renamer
import           Optics

--------------------------------------------------------------------------------
-- Top-level

renameText ::
     FilePath
  -> Text
  -> Either ParseRenameError (IsRenamed (Expression Renamed))
renameText fp text = do
  expression <- first ParserErrored (parseText fp text)
  first
    RenamerErrors
    (let (result, (mappings, unresolvedGlobals)) =
           runState
             (runValidateT
                (runRenamer
                   (renameExpression
                      (Env {globals = wiredInGlobals, cursor = id, scope = mempty})
                      expression)))
             mempty
      in fmap (\thing -> IsRenamed {thing, mappings, unresolvedGlobals}) result)

--------------------------------------------------------------------------------
-- Wired-in

wiredInGlobals :: Map Text (GlobalRef Renamed)
wiredInGlobals =
  M.fromList
    [ ("fromInteger", FromIntegerGlobal)
    , ("fromDecimal", FromDecimalGlobal)
    , ("map", FunctionGlobal MapFunction)
    , ("filter", FunctionGlobal FilterFunction)
    , ("length", FunctionGlobal LengthFunction)
    , ("null", FunctionGlobal NullFunction)
    , ("vega", FunctionGlobal VegaFunction)
    , ("sum", FunctionGlobal SumFunction)
    , ("average", FunctionGlobal AverageFunction)
    , ("distinct", FunctionGlobal DistinctFunction)
    , ("minimum", FunctionGlobal MinimumFunction)
    , ("maximum", FunctionGlobal MaximumFunction)
    , ("sort", FunctionGlobal SortFunction)
    , ("find", FunctionGlobal FindFunction)
    ]

--------------------------------------------------------------------------------
-- Renamers

renameExpression :: Env -> Expression Parsed -> Renamer (Expression Renamed)
renameExpression env =
  \case
    LiteralExpression literal -> renameLiteral env literal
    LambdaExpression lambda -> fmap LambdaExpression (renameLambda env lambda)
    RecordExpression record -> fmap RecordExpression (renameRecord env record)
    PropExpression prop -> fmap PropExpression (renameProp env prop)
    ArrayExpression array -> fmap ArrayExpression (renameArray env array)
    VariantExpression variant -> fmap VariantExpression (renameVariant env variant)
    LetExpression let' -> fmap LetExpression (renameLet env let')
    CaseExpression case' -> fmap CaseExpression (renameCase env case')
    IfExpression if' -> fmap IfExpression (renameIf env if')
    InfixExpression infix' -> fmap InfixExpression (renameInfix env infix')
    ApplyExpression apply -> fmap ApplyExpression (renameApply env apply)
    VariableExpression variable -> renameVariable env variable
    HoleExpression Hole {..} -> do
      final <- finalizeCursor (cursor env) TypeCursor location
      pure (HoleExpression Hole {location = final, typ = Nothing})
    GlobalExpression {} -> error "impossible" -- TODO: Make impossible at type-level.
    -- Use:
    --
    -- data StagedGlobal s where
    --   ParsedGlobal :: Void -> StagedGlobal Parsed
    --   RenamedGlobal :: Global s -> StagedGlobal Renamed
    --   FilledGlobal :: Global s -> StagedGlobal Filled
    --   GeneratedGlobal :: Global s -> StagedGlobal Generated
    --   SolvedGlobal :: Global s -> StagedGlobal Solved
    --   GeneralisedGlobal :: Global s -> StagedGlobal Generalised
    --   ResolvedGlobal :: Global s -> StagedGlobal Resolved
    --
    -- And then use `absurd v` instead of `error "impossible"`.

renameLiteral :: Env -> Literal Parsed -> Renamer (Expression Renamed)
renameLiteral env@Env {cursor} =
  \case
    TextLiteral LiteralText {..} -> do
      final <- finalizeCursor cursor TypeCursor location
      pure (LiteralExpression (TextLiteral LiteralText {location = final, typ = Nothing, ..}))
    NumberLiteral number -> do
      number' <- renameNumber env number
      pure
        (case numberType number' of
           Just typ
             | sigMatchesNumber typ number' ->
               LiteralExpression (NumberLiteral number')
               -- Purely an optimization to avoid a no-op. We could go
               -- further and grow ints/decs to more places if we
               -- wanted.
           _ ->
             ApplyExpression
               Apply
                 { location = BuiltIn
                 , typ = numberType number'
                 , argument = LiteralExpression (NumberLiteral number')
                 , function =
                     GlobalExpression
                       Global
                         { location = BuiltIn
                         , name =
                             let Number {number = someNumber} = number'
                              in case someNumber of
                                   IntegerNumber {} ->
                                     ExactGlobalRef FromIntegerGlobal
                                   DecimalNumber {} ->
                                     ExactGlobalRef FromDecimalGlobal
                         , scheme = RenamedScheme
                         }
                 })
  where
    sigMatchesNumber typ Number {number} =
      case number of
        IntegerNumber {}
          | ConstantType TypeConstant {name = IntegerTypeName} <- typ -> True
        DecimalNumber Decimal {places}
          | ApplyType TypeApplication { function = ConstantType TypeConstant {name = DecimalTypeName}
                                      , argument = ConstantType TypeConstant {name = NatTypeName n}
                                      } <- typ -> n == places
        _ -> False

renameNumber :: Env -> Number Parsed -> Renamer (Number Renamed)
renameNumber env@Env {cursor} Number {..} = do
  final <- finalizeCursor cursor ExpressionCursor location
  typ' <- renameSignature env typ
  pure Number {location = final, typ = typ', ..}

renameLambda :: Env -> Lambda Parsed -> Renamer (Lambda Renamed)
renameLambda env@Env {cursor} Lambda {..} = do
  final <- finalizeCursor cursor ExpressionCursor location
  param' <- renameParam env param
  body' <-
    renameExpression
      (over
         envScopeL
         (LambdaBinding param :)
         (over envCursorL (. LambdaBodyCursor) env))
      body
  typ' <- renameSignature env typ
  pure Lambda {body = body', location = final, param = param', typ = typ', ..}

renameRecord :: Env -> Record Parsed -> Renamer (Record Renamed)
renameRecord env@Env {cursor} Record {..} = do
  final <- finalizeCursor cursor ExpressionCursor location
  fields' <-
    traverse
      (\field@FieldE {name} ->
         renameFieldE (over envCursorL (. RecordFieldCursor name) env) field)
      fields
  typ' <- renameSignature env typ
  pure Record {fields = fields', location = final, typ = typ'}

renameProp :: Env -> Prop Parsed -> Renamer (Prop Renamed)
renameProp env@Env {cursor} Prop {..} = do
  final <- finalizeCursor cursor ExpressionCursor location
  expression' <- renameExpression (over envCursorL (. PropExpressionCursor) env) expression
  typ' <- renameSignature env typ
  pure
    Prop
      { expression = expression'
      , location = final
      , typ = typ'
      , ..
      }

renameArray :: Env -> Array Parsed -> Renamer (Array Renamed)
renameArray env@Env {cursor} Array {..} = do
  final <- finalizeCursor cursor ExpressionCursor location
  expressions' <-
    V.imapM
      (\i -> renameExpression (over envCursorL (. ArrayElementCursor i) env))
      expressions
  typ' <- renameSignature env typ
  pure Array {expressions = expressions', location = final, typ = typ', ..}

renameVariant :: Env -> Variant Parsed -> Renamer (Variant Renamed)
renameVariant env@Env {cursor} Variant {..} = do
  final <- finalizeCursor cursor ExpressionCursor location
  argument' <-
    traverse
      (renameExpression (over envCursorL (. VariantElementCursor) env))
      argument
  typ' <- renameSignature env typ
  pure Variant {argument = argument', location = final, typ = typ', ..}

renameFieldE :: Env -> FieldE Parsed -> Renamer (FieldE Renamed)
renameFieldE env@Env {cursor} FieldE {..} = do
  final <- finalizeCursor cursor TypeCursor location
  expression' <-
    renameExpression (over envCursorL (. RowFieldExpression) env) expression
  pure FieldE {location = final, expression = expression', ..}

-- TODO: Disable duplicate names in bind list.
renameLet :: Env -> Let Parsed -> Renamer (Let Renamed)
renameLet env@Env {cursor} Let {..} = do
  final <- finalizeCursor cursor ExpressionCursor location
  binds' <-
    traverse
      (\(index, bind) ->
         renameBind
           (over
              envScopeL
              (LetBinding (fmap (\Bind {param} -> param) binds) :)
              (over envCursorL (. LetBindCursor (IndexInLet index)) env))
           bind)
      (NE.zip (NE.fromList [0 ..]) binds)
  body' <-
    renameExpression
      (over
         envScopeL
         (LetBinding (fmap (\Bind {param} -> param) binds) :)
         (over envCursorL (. LetBodyCursor) env))
      body
  typ' <- renameSignature env typ
  pure Let {body = body', location = final, binds = binds', typ = typ', ..}

renameCase :: Env -> Case Parsed -> Renamer (Case Renamed)
renameCase env@Env {cursor} Case {..} = do
  final <- finalizeCursor cursor ExpressionCursor location
  typ' <- renameSignature env typ
  scrutinee' <- renameExpression env scrutinee
  alternatives' <- traverse (renameAlternative env) alternatives
  pure
    Case
      { location = final
      , typ = typ'
      , alternatives = alternatives'
      , scrutinee = scrutinee'
      , ..
      }

renameAlternative :: Env -> Alternative Parsed -> Renamer (Alternative Renamed)
renameAlternative env@Env {cursor} Alternative {..} = do
  final <- finalizeCursor cursor ExpressionCursor location
  pattern'' <- renamePattern env pattern'
  let addParam =
        case patternParam pattern' of
          Nothing -> id
          Just param -> over envScopeL (CaseBinding param :)
  expression' <- renameExpression (addParam env) expression
  pure
    Alternative
      {pattern' = pattern'', expression = expression', location = final, ..}

renamePattern :: Env -> Pattern Parsed -> Renamer (Pattern Renamed)
renamePattern env =
  \case
    ParamPattern param -> fmap ParamPattern (renameParam env param)
    VariantPattern variant -> fmap VariantPattern (renameVariantP env variant)

bindingParam :: Binding s -> NonEmpty (Param s)
bindingParam =
  \case
    LambdaBinding p -> pure p
    LetBinding p -> p
    CaseBinding p -> pure p

patternParam :: Pattern s -> Maybe (Param s)
patternParam =
  \case
    ParamPattern param -> pure param
    VariantPattern VariantP {argument} -> argument

renameVariantP :: Env -> VariantP Parsed -> Renamer (VariantP Renamed)
renameVariantP env@Env {cursor} VariantP {..} = do
  final <- finalizeCursor cursor ExpressionCursor location
  argument' <- traverse (renameParam env) argument
  pure VariantP {location = final, argument = argument', ..}

renameInfix :: Env -> Infix Parsed -> Renamer (Infix Renamed)
renameInfix env@Env {cursor} Infix {..} = do
  final <- finalizeCursor cursor ExpressionCursor location
  global' <- renameGlobal (over envCursorL (. InfixOpCursor) env) global
  left' <- renameExpression (over envCursorL (. InfixLeftCursor) env) left
  right' <- renameExpression (over envCursorL (. InfixRightCursor) env) right
  typ' <- renameSignature env typ
  pure
    Infix
      { left = left'
      , global = global'
      , right = right'
      , location = final
      , typ = typ'
      , ..
      }

renameGlobal :: Env -> Global Parsed -> Renamer (Global Renamed)
renameGlobal Env {cursor} Global {..} = do
  final <- finalizeCursor cursor ExpressionCursor location
  let op = pure . NumericBinOpGlobal
  name' <-
    case name of
      "*" -> op MulitplyOp
      "+" -> op AddOp
      "-" -> op SubtractOp
      "/" -> op DivideOp
      "=" -> pure (EqualGlobal Equal)
      "/=" -> pure (EqualGlobal NotEqual)
      ">" -> pure (CompareGlobal GreaterThan)
      "<" -> pure (CompareGlobal LessThan)
      "<=" -> pure (CompareGlobal LessEqualTo)
      ">=" -> pure (CompareGlobal GreaterEqualTo)
      _ -> Renamer (refute (pure (BUG_UnknownOperatorName name)))
  pure Global {location = final, scheme = RenamedScheme, name = ExactGlobalRef name'}

renameBind :: Env -> Bind Parsed -> Renamer (Bind Renamed)
renameBind env@Env {cursor} Bind {param, value, location, typ} = do
  final <- finalizeCursor cursor ExpressionCursor location
  param' <- renameParam env param
  value' <- renameExpression env value
  typ' <- renameSignature env typ
  pure Bind {value = value', param = param', location = final, typ = typ'}

renameApply :: Env -> Apply Parsed -> Renamer (Apply Renamed)
renameApply env@Env {cursor} Apply {..} = do
  function' <-
    renameExpression (over envCursorL (. ApplyFuncCursor) env) function
  argument' <-
    renameExpression (over envCursorL (. ApplyArgCursor) env) argument
  final <- finalizeCursor cursor ExpressionCursor location
  typ' <- renameSignature env typ
  pure Apply {function = function', argument = argument', location = final, typ = typ'}

renameIf :: Env -> If Parsed -> Renamer (If Renamed)
renameIf env@Env {cursor} If {..} = do
  condition' <- renameExpression (over envCursorL (. IfCursor) env) condition
  consequent' <- renameExpression (over envCursorL (. IfCursor) env) consequent
  alternative' <- renameExpression (over envCursorL (. IfCursor) env) alternative
  final <- finalizeCursor cursor ExpressionCursor location
  typ' <- renameSignature env typ
  pure
    If
      { consequent = consequent'
      , alternative = alternative'
      , condition = condition'
      , location = final
      , typ = typ'
      }

renameVariable ::
     Env
  -> Variable Parsed
  -> Renamer (Expression Renamed)
renameVariable env@Env {scope, cursor, globals} variable@Variable { name
                                                                  , location
                                                                  , typ
                                                                  } =
  case find
         (any (\Param {name = name'} -> name' == name) . bindingParam . snd)
         (zip [0 ..] scope) of
    Nothing -> do
      final <- finalizeCursor cursor ExpressionCursor location
      case M.lookup name globals of
        Nothing -> do
          modify (over _2 (Set.insert name))
          pure
            (GlobalExpression
               (Global
                  { location = final
                  , name = UnresolvedGlobal name
                  , scheme = RenamedScheme
                  }))
        Just globalRef -> do
          pure
            (GlobalExpression
               (Global
                  { location = final
                  , name = ResolvedGlobalRef name globalRef
                  , scheme = RenamedScheme
                  }))
    Just (index, binding) -> do
      final <- finalizeCursor cursor ExpressionCursor location
      typ' <- renameSignature env typ
      deBrujinIndex <-
        case binding of
          LambdaBinding {} -> pure (DeBrujinIndex (DeBrujinNesting index))
          CaseBinding {} -> pure (DeBrujinIndex (DeBrujinNesting index))
          LetBinding params ->
            case findIndex
                   (\Param {name = name'} -> name' == name)
                   (toList params) of
              Nothing ->
                Renamer (refute (pure (BUG_MissingVariable scope globals variable)))
              Just subIndex ->
                pure
                  (DeBrujinIndexOfLet
                     (DeBrujinNesting index)
                     (IndexInLet subIndex))
      pure
        (VariableExpression
           (Variable {location = final, name = deBrujinIndex, typ = typ'}))

renameParam :: Env -> Param Parsed -> Renamer (Param Renamed)
renameParam env@Env{cursor} Param {..} = do
  final <- finalizeCursor cursor LambdaParamCursor location
  typ' <- renameSignature env typ
  pure Param {name = (), location = final, typ = typ'}

renameSignature :: Env -> Maybe (Type Parsed) -> Renamer (Maybe (Type Renamed))
renameSignature env =
  maybe
    (pure Nothing)
    (fmap Just . renameType (over envCursorL (. SignatureCursor) env))

renameType :: Env -> Type Parsed -> Renamer (Type Renamed)
renameType env@Env {cursor} =
  \case
    FreshType location -> do
      final <- finalizeCursor cursor LambdaParamCursor location
      pure (FreshType final)
    VariableType typeVariable ->
      fmap VariableType (renameTypeVariable env typeVariable)
    ApplyType typeApplication ->
      fmap ApplyType (renameTypeApplication env typeApplication)
    ConstantType typeConstant ->
      fmap ConstantType (renameTypeConstant env typeConstant)
    RowType typeRow -> fmap RowType (renameTypeRow env typeRow)
    RecordType typeRow -> fmap RecordType (renameType env typeRow)
    VariantType typeRow -> fmap VariantType (renameType env typeRow)
    ArrayType typ -> fmap ArrayType (renameType env typ)

renameTypeConstant :: Env -> TypeConstant Parsed -> Renamer (TypeConstant Renamed)
renameTypeConstant Env{cursor} TypeConstant {..} = do
  final <- finalizeCursor cursor TypeCursor location
  pure TypeConstant {location = final, ..}

renameTypeRow :: Env -> TypeRow Parsed -> Renamer (TypeRow Renamed)
renameTypeRow env@Env {cursor} TypeRow {..} = do
  final <- finalizeCursor cursor TypeCursor location
  fields' <-
    traverse (renameField (over envCursorL (. RowFieldCursor) env)) fields
  mtypeVariable <-
    case typeVariable of
      Nothing -> pure Nothing
      Just typeVariable' -> fmap pure (renameTypeVariable env typeVariable')
  pure
    TypeRow {location = final, fields = fields', typeVariable = mtypeVariable}

renameField :: Env -> Field Parsed -> Renamer (Field Renamed)
renameField env@Env{cursor} Field {..} = do
  final <- finalizeCursor cursor TypeCursor location
  typ' <- renameType (over envCursorL (. RowFieldType) env) typ
  pure Field {location = final, typ = typ', ..}

renameTypeApplication :: Env -> TypeApplication Parsed -> Renamer (TypeApplication Renamed)
renameTypeApplication env@Env {cursor} TypeApplication {function, argument, ..} = do
  function' <- renameType (over envCursorL (. TypeApplyCursor) env) function
  argument' <- renameType (over envCursorL (. TypeApplyCursor) env) argument
  final <- finalizeCursor cursor TypeCursor location
  pure
    TypeApplication
      {function = function', argument = argument', location = final, ..}

renameTypeVariable :: Env -> TypeVariable Parsed -> Renamer (TypeVariable Renamed)
renameTypeVariable Env{cursor} TypeVariable {..} = do
  final <- finalizeCursor cursor TypeCursor location
  pure TypeVariable {location = final, ..}

--------------------------------------------------------------------------------
-- Cursor operations

finalizeCursor :: CursorBuilder -> Cursor -> StagedLocation Parsed -> Renamer Cursor
finalizeCursor cursor finalCursor loc = do
  modify (over _1 (M.insert final loc))
  pure final
  where final = cursor finalCursor
