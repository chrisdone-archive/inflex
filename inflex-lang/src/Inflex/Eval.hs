{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE RecordWildCards, PatternSynonyms #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns, DuplicateRecordFields #-}

-- |

module Inflex.Eval
  ( Eval(..)
  , evalTextDefaulted
  , evalTextRepl
  , evalDefaulted
  , DefaultEvalError(..)
  ) where

import           Control.Monad
import           Data.Bifunctor
import qualified Data.ByteString.Lazy.Builder as SB
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Containers.ListUtils
import           Data.Foldable
import           Data.Functor.Identity
import qualified Data.List as List
import           Data.List.Extra
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Text (Text)
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           GHC.Natural
import           Inflex.Decimal
import           Inflex.Defaulter
import           Inflex.Derived
import           Inflex.Display ()
import           Inflex.Renamer (patternParam)
import           Inflex.Type
import           Inflex.Types
import           Inflex.Types as Array (Array(..))
import           Inflex.Types.Eval
import           Inflex.Types.Optics
import           Inflex.Variants
import           Lexx
import           Optics (set, (%), element, Is, Optic, A_Setter)
import qualified RIO
import           RIO (RIO, glog)


--------------------------------------------------------------------------------
-- Entry points

evalTextRepl :: Bool -> Text -> IO ()
evalTextRepl loud text = do
  lastRef <- RIO.newSomeRef ""
  output <-
    RIO.runRIO
      Eval
        { glogfunc =
            if loud
              then RIO.mkGLogFunc
                     (\stack msg ->
                        case msg of
                          EvalStep e -> do
                            prev <- RIO.readSomeRef lastRef
                            let cur =
                                  SB.toLazyByteString
                                    (RIO.getUtf8Builder (RIO.display e))
                            unless
                              (prev == cur)
                              (do L8.putStrLn cur
                                  RIO.writeSomeRef lastRef cur)
                          _ -> print stack >> prettyWrite msg)
              else mempty
        , globals = mempty
        }
      (evalTextDefaulted mempty "" text)
  case output of
    Left e -> print (e :: DefaultEvalError ())
    Right e -> do
      putStrLn "=>"
      L8.putStrLn (SB.toLazyByteString (RIO.getUtf8Builder (RIO.display e)))

evalTextDefaulted ::
     Map Hash (Either e (Scheme Polymorphic))
  -> FilePath
  -> Text
  -> RIO Eval (Either (DefaultEvalError e) (Expression Resolved))
evalTextDefaulted schemes fp text = do
  result <-
    RIO.runRIO
      DefaulterReader
      (fmap (first DefaulterErrored) (defaultText schemes fp text))
  case result of
    Right Cell {defaulted} -> fmap Right (evalExpression id defaulted)
    Left e -> pure (Left e)

evalDefaulted ::
     Cell
  -> RIO Eval (Either (DefaultEvalError e) (Expression Resolved))
evalDefaulted Cell{defaulted} = do
  fmap Right (evalExpression id defaulted)

--------------------------------------------------------------------------------
-- Expression evaluator

evalExpression ::
     (Expression Resolved -> Expression Resolved)
  -> Expression Resolved
  -> RIO Eval (Expression Resolved)
evalExpression build expression = do
  glog (EvalStep (build expression))
  result <- case expression of
    -- Self-evaluating forms:
    LiteralExpression {} -> pure expression
    LambdaExpression {} -> pure expression
    VariableExpression {} -> pure expression
    HoleExpression {} -> pure expression
    -- Data structures:
    RecordExpression record -> evalRecord build record
    ArrayExpression array -> evalArray build array
    VariantExpression variant -> evalVariant build variant
    -- Globals:
    GlobalExpression global -> evalGlobal build global
    -- Apply:
    ApplyExpression apply -> evalApply build apply
    InfixExpression infix' -> evalInfix build infix'
    -- Special forms:
    IfExpression if' -> evalIf build if'
    CaseExpression case' -> evalCase build case'
    PropExpression prop -> evalProp build prop
    -- Disabled forms:
    BoundaryExpression {} -> pure expression
    EarlyExpression {} -> pure expression
    LetExpression {} -> pure expression
  glog (EvalStep (build result))
  pure result

--------------------------------------------------------------------------------
-- Case matching

evalCase ::
     (Expression Resolved -> Expression Resolved)
  -> Case Resolved
  -> RIO Eval (Expression Resolved)
evalCase build case'@Case {..} = do
  scrutinee' <-
    evalExpression
      (build . CaseExpression . setFlipped caseScrutineeL case')
      scrutinee
  case listToMaybe (mapMaybe (match scrutinee') (toList alternatives)) of
    Just e -> evalExpression build e
    Nothing ->
      pure
        (CaseExpression
           (Case {scrutinee = scrutinee', location = SteppedCursor, ..}))

-- | Finds a match, if any, and the result must be evaled again.
match :: Expression Resolved -> Alternative Resolved -> Maybe (Expression Resolved)
match scrutinee Alternative {..} =
  case pattern' of
    ParamPattern {} -> pure (runIdentity (betaReduce expression scrutinee))
    VariantPattern VariantP {tag = expectedTag, argument = namedArgument} ->
      case scrutinee of
        VariantExpression Variant {tag = actualTag, argument = actualArgument} ->
          if expectedTag == actualTag
            then case namedArgument of
                   Nothing -> pure expression
                   Just {}
                     | Just slot <- actualArgument ->
                       pure (runIdentity (betaReduce expression slot))
                     | otherwise -> Nothing -- TODO: This would indicate a bug.
            else Nothing
        _ -> Nothing

--------------------------------------------------------------------------------
-- Data structures

evalProp ::
     (Expression Resolved -> Expression Resolved)
  -> Prop Resolved
  -> RIO Eval (Expression Resolved)
evalProp build prop@Prop {..} = do
  expression' <-
    evalExpression
      (build . PropExpression . setFlipped propExpressionL prop)
      expression
  case expression' of
    RecordExpression Record {fields} ->
      case find (\FieldE {name = name'} -> name' == name) fields of
        Nothing -> pure expression0
        Just FieldE {expression = v} -> evalExpression build v
    _ -> pure expression0
  where
    expression0 = PropExpression prop

evalRecord ::
     (Expression Resolved -> Expression Resolved)
  -> Record Resolved
  -> RIO Eval (Expression Resolved)
evalRecord build record0@Record {..} = do
  (_, fields') <-
    mapAccumM
      (\record (i, FieldE {expression, name}) -> do
         e' <-
           evalExpression
             (build .
              RecordExpression .
              setFlipped (recordFieldsL % element i % fieldEExpressionL) record)
             expression
         let field' = FieldE {location = SteppedCursor, expression = e', ..}
         pure (set (recordFieldsL % element i) field' record, field'))
      record0
      (zip [0 ..] fields)
  pure (RecordExpression (Record {fields = fields', ..}))

evalArray ::
     (Expression Resolved -> Expression Resolved)
  -> Array Resolved
  -> RIO Eval (Expression Resolved)
evalArray build array0@Array {..} =
  case form of
    Evaluated -> pure (ArrayExpression array0)
    Unevaluated -> do
      (_, expressions') <-
        mapAccumM
          (\array (i, expression) -> do
             e' <-
               evalExpression
                 (build .
                  ArrayExpression .
                  setFlipped (arrayExpressionsL % element i) array)
                 expression
             pure (set (arrayExpressionsL % element i) e' array, e'))
          array0
          (V.indexed expressions)
      pure
        (ArrayExpression
           Array {expressions = expressions', form = Evaluated, ..})

evalVariant ::
     (Expression Resolved -> Expression Resolved)
  -> Variant Resolved
  -> RIO Eval (Expression Resolved)
evalVariant build variant@Variant {..} = do
  argument' <-
    traverse
      (evalExpression
         (build . VariantExpression . setFlipped variantArgumentL variant . pure))
      argument
  pure (VariantExpression Variant {argument = argument', ..})

--------------------------------------------------------------------------------
-- Globals

evalGlobal ::
     (Expression Resolved -> Expression Resolved)
  -> Global Resolved
  -> RIO Eval (Expression Resolved)
evalGlobal build global@Global {name} = do
  Eval {globals} <- RIO.ask
  case name of
    HashGlobal hash ->
      case M.lookup hash globals of
        Just expression -> evalExpression build expression
        Nothing -> do
          glog (GlobalMissing global)
          pure (GlobalExpression global)
    _ -> pure (GlobalExpression global)

--------------------------------------------------------------------------------
-- Apply

-- | Try to apply a lambda, otherwise apply primitive functions.
evalApply ::
     (Expression Resolved -> Expression Resolved)
  -> Apply Resolved
  -> RIO Eval (Expression Resolved)
evalApply build apply@Apply {function, argument, ..} = do
  function' <-
    evalExpression
      (build . ApplyExpression . setFlipped applyFunctionL apply)
      function
  argument' <-
    evalExpression
      (build .
       ApplyExpression .
       setFlipped applyArgumentL (setFlipped applyFunctionL apply function'))
      argument
  case function' of
    LambdaExpression Lambda {body} -> do
      body' <- betaReduce body argument'
      evalExpression build body'
    _ ->
      ignore $ evalApplyNF
        (ApplyExpression Apply {function = function', argument = argument', ..})

-- | Apply after the function and argument are reduced to normal form.
evalApplyNF :: Expression Resolved -> RIO Eval (Expression Resolved)
evalApplyNF expression =
  case expression of
    ApplyMethod1 FromIntegerGlobal instance' argument@(IntegerAtom integer typ) ->
      evalFromInteger typ argument integer instance'
    ApplyMethod1 FromDecimalGlobal (FromDecimalDecimalInstance instance') (DecimalAtom decimal typ) ->
      evalFromDecimal typ expression decimal instance'
    -- Derived:
    ApplyFunction1 NullFunction argument ->
      apply1 nullFunction argument (expressionType expression)
    ApplyFunction1 NotFunction argument ->
      apply1 notFunction argument (expressionType expression)
    ApplyFunction2 FromOkFunction default' argument ->
      apply2 from_okFunction default' argument (expressionType expression)
    ApplyFunction2 AnyFunction p array ->
      apply2 anyFunction p array (expressionType expression)
    ApplyFunction2 AllFunction p array ->
      apply2 allFunction p array (expressionType expression)
    -- Primitives:
    ApplyFunction1 ConcatFunction (ArrayExpression array) ->
      evalConcat array expression
    ApplyFunction2 LengthFunction fromInteger' (ArrayExpression array) ->
      evalLength fromInteger' array (expressionType expression)
    ApplyFunction2 SortFunction _comparator (ArrayExpression array) ->
      evalSort expression array
    ApplyFunction2 DistinctFunction _comparator (ArrayExpression array) ->
      evalDistinct expression array
    ApplyFunction2 FilterFunction predicate (ArrayExpression array) ->
      evalFilter expression predicate array
    ApplyFunction2 MapFunction function (ArrayExpression array) ->
      evalMap expression function array
    -- Lookups:
    ApplyFunction2 FindFunction function (ArrayExpression array) ->
      evalFind function array expression
    -- Sum:
    ApplyFold1 SumFunction IntegerOpInstance {} array ->
      evalAtomicFoldInteger V.sum array expression "sum_empty"
    ApplyFold1 SumFunction (DecimalOpInstance places _) array ->
      evalAtomicFoldDecimal places (V.foldl1' plus) array expression "sum_empty"
    -- Maximum:
    ApplyFold0 MaximumFunction CompareIntegerInstance{} array ->
      evalAtomicFoldInteger V.maximum array expression "maximum_empty"
    ApplyFold0 MaximumFunction (CompareDecimalInstance places) array ->
      evalAtomicFoldDecimal
        places
        (V.foldl1' max)
        array
        expression
        "maximum_empty"
    -- Minimum:
    ApplyFold0 MinimumFunction CompareIntegerInstance {} array ->
      evalAtomicFoldInteger V.minimum array expression "minimum_empty"
    ApplyFold0 MinimumFunction (CompareDecimalInstance places) array ->
      evalAtomicFoldDecimal
        places
        (V.foldl1' min)
        array
        expression
        "minimum_empty"
    -- Average:
    ApplyFold2 AverageFunction IntegerOpInstance {} array ->
      evalAtomicFoldInteger
        (\vs -> V.sum vs `div` fromIntegral (V.length vs))
        array
        expression
        "average_empty"
    ApplyFold2 AverageFunction (DecimalOpInstance places _) array ->
      evalAtomicFoldDecimal
        places
        (\vs ->
           V.foldl1' plus vs `divide`
           decimalFromInteger (fromIntegral (V.length vs)) places)
        array
        expression
        "average_empty"
    _ -> pure expression

--------------------------------------------------------------------------------
-- Infix

evalInfix ::
     (Expression Resolved -> Expression Resolved)
  -> Infix Resolved
  -> RIO Eval (Expression Resolved)
evalInfix build infix0@Infix {..} = do
  global' <- ignore (evalExpression id global)
  let updateLeft = setFlipped infixLeftL infix0
  left' <- evalExpression (build . InfixExpression . updateLeft) left
  right' <-
    evalExpression
      (build . InfixExpression . setFlipped infixRightL (updateLeft left'))
      right
  let expression =
        InfixExpression
          Infix
            { global = global'
            , left = left'
            , right = right'
            , location = SteppedCursor
            , ..
            }
  case (left', right') of
    (HoleExpression {}, _) -> pure expression
    (_, HoleExpression {}) -> pure expression
    _ -> applyInfixNF expression global' left' right'

applyInfixNF ::
     Expression Resolved
  -> Expression Resolved
  -> Expression Resolved
  -> Expression Resolved
  -> RIO Eval (Expression Resolved)
applyInfixNF expression global left right =
  case global of
    ApplyDict1 NumericBinOpGlobal{} (IntegerOpInstance numericBinOp)  ->
      evalIntegerOp expression numericBinOp left right
    ApplyDict1 NumericBinOpGlobal{} (DecimalOpInstance precision numericBinOp)  ->
      evalDecimalOp expression precision numericBinOp left right
    ApplyDict1 (EqualGlobal equality) _instance ->
      evalAtomicEquality expression equality left right
    ApplyDict1 (CompareGlobal comparison) _instance ->
      evalAtomicComparison expression comparison left right
    _ -> pure expression

--------------------------------------------------------------------------------
-- Equality

evalAtomicEquality ::
     Applicative f
  => Expression Resolved
  -> Equality
  -> Expression Resolved
  -> Expression Resolved
  -> f (Expression Resolved)
evalAtomicEquality expression equality left' right' =
  case (left', right') of
    (LiteralExpression (NumberLiteral Number {number = left}), LiteralExpression (NumberLiteral Number {number = right})) -> do
      pure
        (if comparator left right
        then trueVariant location
        else falseVariant location)
    (LiteralExpression (TextLiteral LiteralText {text = left}), LiteralExpression (TextLiteral LiteralText {text = right})) -> do
      pure
        (if comparator left right
        then trueVariant location
        else falseVariant location)
    _ -> pure expression
  where
    location = SteppedCursor
    comparator :: Eq a => a -> a -> Bool
    comparator =
      case equality of
        Equal -> (==)
        NotEqual -> (/=)

--------------------------------------------------------------------------------
-- Ordering

evalAtomicComparison ::
     Applicative f
  => Expression Resolved
  -> Comparison
  -> Expression s1
  -> Expression s2
  -> f (Expression Resolved)
evalAtomicComparison expression compareity left' right' =
  case (left', right') of
    (LiteralExpression (NumberLiteral Number {number = left}), LiteralExpression (NumberLiteral Number {number = right})) -> do
      pure
        (if comparator left right
           then trueVariant location
           else falseVariant location)
    (LiteralExpression (TextLiteral LiteralText {text = left}), LiteralExpression (TextLiteral LiteralText {text = right})) -> do
      pure
        (if comparator left right
           then trueVariant location
           else falseVariant location)
    _ -> pure expression
  where
    location = SteppedCursor
    comparator :: Ord a => a -> a -> Bool
    comparator =
      case compareity of
        GreaterThan -> (>)
        LessThan -> (<)
        GreaterEqualTo -> (>=)
        LessEqualTo -> (<=)

--------------------------------------------------------------------------------
-- Numeric operations

evalIntegerOp ::
     Expression Resolved
  -> NumericBinOp
  -> Expression Resolved
  -> Expression Resolved
  -> RIO Eval (Expression Resolved)
evalIntegerOp expression numericBinOp left' right' =
  case (left', right') of
    (IntegerAtom left typ, IntegerAtom right _typ)
      | DivideOp <- numericBinOp
      , 0 <- right -> pure expression -- Nothing to do for division by zero.
      | otherwise -> do
        pure
          (LiteralExpression
             (NumberLiteral
                Number
                  { number =
                      IntegerNumber
                        (case numericBinOp of
                           AddOp -> left + right
                           SubtractOp -> left - right
                           MulitplyOp -> left * right
                           DivideOp -> div left right)
                  , location = SteppedCursor
                  , ..
                  }))
    _ -> pure expression

evalDecimalOp ::
     Expression Resolved
  -> Natural
  -> NumericBinOp
  -> Expression Resolved
  -> Expression Resolved
  -> RIO Eval (Expression Resolved)
evalDecimalOp expression places numericBinOp left' right' =
  case (left', right') of
    (DecimalAtom x typ, DecimalAtom y _typ) -> do
      case case numericBinOp of
             AddOp -> pure (x `plus` y)
             SubtractOp -> pure (x `minus` y)
             MulitplyOp -> pure (x `multiply` y)
             DivideOp ->
               if y == decimalFromInteger 0 places
                 then Left () -- We stop due to division by zero.
                 else pure (x `divide` y) of
        Left () -> pure expression -- Division by zero has no answer, so we stop.
        Right result ->
          pure
            (LiteralExpression
               (NumberLiteral
                  Number
                    { number = DecimalNumber result
                    , location = SteppedCursor
                 -- TODO: Add "step number X" or something to say
                 -- "this is where the value came from". Could be
                 -- useful for finding 0's which hit an x/0.
                    , ..
                    }))
    _ -> pure expression

--------------------------------------------------------------------------------
-- Special forms

evalIf ::
     (Expression Resolved -> Expression Resolved)
  -> If Resolved
  -> RIO Eval (Expression Resolved)
evalIf build if'@If {..} = do
  condition' <-
    evalExpression
      (build . IfExpression . setFlipped ifConditionL if')
      condition
  case condition' of
    BoolAtom bool ->
      if bool
        then evalExpression build consequent
        else evalExpression build alternative
    _ ->
      pure
        (IfExpression If {condition = condition', location = SteppedCursor, ..})

--------------------------------------------------------------------------------
-- Applying functions easily

apply1 ::
     Expression Resolved
  -> Expression Resolved
  -> Type Generalised
  -> RIO Eval (Expression Resolved)
apply1 function argument typ =
  ignore $ evalApply
    id
    Apply {location = BuiltIn, function, argument, typ, style = EvalApply}

apply2 ::
     Expression Resolved
  -> Expression Resolved
  -> Expression Resolved
  -> Type Generalised
  -> RIO Eval (Expression Resolved)
apply2 function argument1 argument2 typ =
  ignore $ evalApply
    id
    Apply
      { location = BuiltIn
      , function =
          ApplyExpression
            Apply
              { function
              , typ
              , argument = argument1
              , location = BuiltIn
              , style = EvalApply
              }
      , typ
      , argument = argument2
      , style = EvalApply
      }

--------------------------------------------------------------------------------
-- Reification/reflection

pattern BoolAtom :: Bool -> Expression Resolved
pattern BoolAtom bool <-
  VariantExpression Variant {tag = TagName ((== "true") -> bool)}
  where
    BoolAtom bool = VariantExpression Variant
      { tag = TagName
                (if bool
                    then "true"
                    else "false")
      , argument = Nothing
      , location = BuiltIn
      , typ = boolType BuiltIn
      }

pattern TextAtom :: Text -> Expression Resolved
pattern TextAtom text <-
  LiteralExpression (TextLiteral (LiteralText { text}))
  where
    TextAtom text  =
      LiteralExpression
        (TextLiteral
          (LiteralText {text, location = BuiltIn, typ = textT}))

pattern IntegerAtom :: Integer -> Type Generalised -> Expression Resolved
pattern IntegerAtom integer typ <-
  LiteralExpression (NumberLiteral (Number { number = IntegerNumber integer
                                           , typ
                                           }))
  where
    IntegerAtom integer typ =
      LiteralExpression
        (NumberLiteral
          (Number
            { number = IntegerNumber integer
            , typ
            , location = BuiltIn
            }))

pattern DecimalAtom :: Decimal -> Type Generalised -> Expression Resolved
pattern DecimalAtom decimal typ <-
  LiteralExpression (NumberLiteral (Number { number = DecimalNumber decimal
                                           , typ
                                           }))
  where
    DecimalAtom decimal typ =
      LiteralExpression
        (NumberLiteral
          (Number
            { number = DecimalNumber decimal
            , typ
            , location = BuiltIn
            }))

--------------------------------------------------------------------------------
-- Reification

data Reified e
  = TextR e !Text
  | IntegerR e !Integer
  | DecimalR e !Decimal
  | BoolR e !Bool
  | HoleR e
  deriving (Functor, Traversable, Foldable)

instance Eq (Reified e) where
  (==) x y =
    case (x, y) of
      (TextR _ a, TextR _ b) -> a == b
      (IntegerR _ a, IntegerR _ b) -> a == b
      (DecimalR _ a, DecimalR _ b) -> a == b
      (BoolR _ a, BoolR _ b) -> a == b
      _ -> error "Invalid Eq comparison between types."

instance Ord (Reified e) where
  compare x y =
    case (x, y) of
      (TextR _ a, TextR _ b) -> a `compare` b
      (IntegerR _ a, IntegerR _ b) -> a `compare` b
      (DecimalR _ a, DecimalR _ b) -> a `compare` b
      (BoolR _ a, BoolR _ b) -> a `compare` b
      _ -> error "Invalid Ord comparison between types."

originalR :: Reified e -> e
originalR =
  \case
    TextR e _ -> e
    IntegerR e _ -> e
    DecimalR e _ -> e
    BoolR e _ -> e
    HoleR e -> e

reify :: Expression Resolved -> Reified (Expression Resolved)
reify e' =
  case e' of
    BoolAtom bool -> BoolR e' bool
    TextAtom text -> TextR e' text
    IntegerAtom integer _ -> IntegerR e' integer
    DecimalAtom decimal _ -> DecimalR e' decimal
    _ -> HoleR e'

--------------------------------------------------------------------------------
-- Apply patterns up to arity-4

pattern ApplyGlobal1 :: GlobalRef Resolved -> Expression Resolved -> Expression Resolved
pattern ApplyGlobal1 name argument <-
  ApplyExpression Apply {function = GlobalExpression Global{name}, argument}

pattern ApplyMethod1 :: GlobalRef Resolved -> InstanceName -> Expression Resolved -> Expression Resolved
pattern ApplyMethod1 method' instance' argument <-
  ApplyExpression Apply{function=ApplyGlobal1 method' (GlobalExpression Global{name=InstanceGlobal instance'}), argument}

pattern ApplyDict1 :: GlobalRef Resolved -> InstanceName -> Expression Resolved
pattern ApplyDict1 method' instance' <-
  ApplyGlobal1 method' (GlobalExpression Global{name=InstanceGlobal instance'})

pattern ApplyGlobal2 :: GlobalRef Resolved -> Expression Resolved -> Expression Resolved -> Expression Resolved
pattern ApplyGlobal2 global argument1 argument2 <-
  ApplyExpression Apply {function = ApplyGlobal1 global argument1, argument=argument2}

pattern ApplyFunction1 :: Function -> Expression Resolved -> Expression Resolved
pattern ApplyFunction1 function argument1 <-
  ApplyGlobal1 (FunctionGlobal function) argument1

pattern ApplyFunction2 :: Function -> Expression Resolved -> Expression Resolved -> Expression Resolved
pattern ApplyFunction2 function argument1 argument2 <-
  ApplyGlobal2 (FunctionGlobal function) argument1 argument2

pattern ApplyFold0 :: Function -> InstanceName -> Array Resolved -> Expression Resolved
pattern ApplyFold0 function instance' array <-
 ApplyExpression Apply {
   argument = ArrayExpression array
  , function = ApplyExpression Apply {
      argument = GlobalExpression Global{name=InstanceGlobal instance'}
     , function = GlobalExpression Global{name=FunctionGlobal function}
     }
  }

pattern ApplyFold1 :: Function -> InstanceName -> Array Resolved -> Expression Resolved
pattern ApplyFold1 function instance' array <-
 ApplyExpression Apply {
   argument = ArrayExpression array
  , function = ApplyExpression Apply {
      argument = _
    , function = ApplyExpression Apply {
        argument = GlobalExpression Global{name=InstanceGlobal instance'}
      , function = GlobalExpression Global{name=FunctionGlobal function}
      }
    }
  }

pattern ApplyFold2 :: Function -> InstanceName -> Array Resolved -> Expression Resolved
pattern ApplyFold2 function instance' array <-
 ApplyExpression Apply {
   argument = ArrayExpression array
  , function = ApplyExpression Apply {
      argument = _fi
    , function = ApplyExpression Apply {
        argument = _div
      , function = ApplyExpression Apply {
            argument = GlobalExpression Global{name=InstanceGlobal instance'}
          , function = GlobalExpression Global{name=FunctionGlobal function}
          }
      }
    }
  }

--------------------------------------------------------------------------------
-- Beta reduction

betaReduce ::
     forall m. Monad m => Expression Resolved -> Expression Resolved -> m (Expression Resolved)
betaReduce body0 arg = go 0 body0
  where
    go :: DeBrujinNesting -> Expression Resolved -> m (Expression Resolved)
    go deBrujinNesting =
      \case
        e@(VariableExpression Variable {name})
          | deBrujinIndexNesting name == deBrujinNesting -> pure arg
          | otherwise -> pure e
        LambdaExpression Lambda {..} -> do
          body' <- go (deBrujinNesting + 1) body
          pure (LambdaExpression Lambda {body = body', ..})
        CaseExpression Case {..} -> do
          scrutinee' <- go deBrujinNesting scrutinee
          alternatives' <-
            traverse
              (\Alternative {location = altloc, ..} -> do
                 expression' <-
                   go
                     (deBrujinNesting +
                     -- TODO: check this arithmetic.
                      case patternParam pattern' of
                        Just {} -> 1
                        Nothing -> 0)
                     expression
                 pure
                   Alternative {expression = expression', location = altloc, ..})
              alternatives
          pure (CaseExpression Case {scrutinee = scrutinee', alternatives = alternatives', ..})
        ApplyExpression Apply {..} -> do
          argument' <- go deBrujinNesting argument
          function' <- go deBrujinNesting function
          pure
            (ApplyExpression
               Apply {argument = argument', function = function', ..})
        IfExpression If {..} -> do
          condition' <- go deBrujinNesting condition
          consequent' <- go deBrujinNesting consequent
          alternative' <- go deBrujinNesting alternative
          pure
            (IfExpression
               If
                 { condition = condition'
                 , consequent = consequent'
                 , alternative = alternative'
                 , ..
                 })
        PropExpression Prop {..} -> do
          expression' <- go deBrujinNesting expression
          pure (PropExpression Prop {expression = expression', ..})
        EarlyExpression Early {..} -> do
          expression' <- go deBrujinNesting expression
          pure (EarlyExpression Early {expression = expression', ..})
        BoundaryExpression Boundary {..} -> do
          expression' <- go deBrujinNesting expression
          pure (BoundaryExpression Boundary {expression = expression', ..})
        ArrayExpression Array {..} -> do
          expressions' <- traverse (go deBrujinNesting) expressions
          pure (ArrayExpression Array {expressions = expressions', ..})
        VariantExpression Variant {..} -> do
          argument' <- traverse (go deBrujinNesting) argument
          pure (VariantExpression Variant {argument = argument', ..})
        RecordExpression Record {..} -> do
          fields' <-
            traverse
              (\FieldE {location = l, ..} -> do
                 e <- go deBrujinNesting expression
                 pure FieldE {location = l, expression = e, ..})
              fields
          pure (RecordExpression Record {fields = fields', ..})
        InfixExpression Infix {..} -> do
          left' <- go deBrujinNesting left
          right' <- go deBrujinNesting right
          global' <- go deBrujinNesting global
          pure
            (InfixExpression
               Infix {left = left', right = right', global = global', ..})
        LetExpression Let {..} -> do
          body' <- go (deBrujinNesting + 1) body
          pure (LetExpression Let {body = body', ..})
        e@GlobalExpression {} -> pure e
        e@LiteralExpression {} -> pure e
        e@HoleExpression {} -> pure e

--------------------------------------------------------------------------------
-- Primitive class methods

evalFromInteger ::
     Type Generalised
  -> Expression Resolved
  -> Integer
  -> InstanceName
  -> RIO Eval (Expression Resolved)
evalFromInteger typ expression integer =
  \case
    FromIntegerIntegerInstance -> pure expression
    FromIntegerDecimalInstance supersetPlaces ->
      pure
        (LiteralExpression
           (NumberLiteral
              (Number
                 { number =
                     DecimalNumber (decimalFromInteger integer supersetPlaces)
                 , typ
                 , location = SteppedCursor
                 , ..
                 })))
    _ -> pure expression

evalFromDecimal ::
     Type Generalised
  -> Expression Resolved
  -> Decimal
  -> FromDecimalInstance
  -> RIO Eval (Expression Resolved)
evalFromDecimal typ expression decimal fromDecimalInstance = do
  mdecimal <- evalFromDecimalMaybe fromDecimalInstance decimal
  case mdecimal of
    Nothing -> pure expression
    Just decimal' ->
      pure
        (LiteralExpression
           (NumberLiteral
              (Number
                 { number = DecimalNumber decimal'
                 , typ
                 , location = SteppedCursor
                 , ..
                 })))

evalFromDecimalMaybe :: FromDecimalInstance -> Decimal -> RIO Eval (Maybe Decimal)
evalFromDecimalMaybe fromDecimalInstance decimal =
  if thisSubsetPlaces == subsetPlaces
    then if thisSubsetPlaces == supersetPlaces
           then pure (Just decimal)
           else if thisSubsetPlaces < supersetPlaces
                  then pure (Just (expandDecimalPrecision supersetPlaces decimal))
                  else do glog
                            (CannotShrinkADecimalFromTo
                               thisSubsetPlaces
                               supersetPlaces)
                          pure Nothing
    else do glog
              (MismatchingPrecisionsInFromDecimal thisSubsetPlaces subsetPlaces)
            pure Nothing
  where
    Decimal {places = thisSubsetPlaces} = decimal
    FromDecimalInstance {supersetPlaces, subsetPlaces} = fromDecimalInstance

--------------------------------------------------------------------------------
-- Primitive functions

evalLength ::
     Expression Resolved
  -> Array Resolved
  -> Type Generalised
  -> RIO Eval (Expression Resolved)
evalLength fromInteger' (Array {expressions}) typ =
  apply2
    (GlobalExpression
       (Global
          { location = SteppedCursor
          , scheme = ResolvedScheme (nullType BuiltIn) -- TODO:
          , name = FromIntegerGlobal
          }))
    fromInteger'
    (LiteralExpression
       (NumberLiteral
          Number
            { number = IntegerNumber (fromIntegral (V.length expressions))
            , location = SteppedCursor
            , typ -- TODO:
            }))
    typ

evalFilter ::
     Expression Resolved
  -> Expression Resolved
  -> Array Resolved
  -> RIO Eval (Expression Resolved)
evalFilter expression predicate (Array {expressions}) = do
  seenHoleRef <- RIO.newSomeRef False
  expressions' <-
    traverse
      (\arrayItem -> do
         bool <-
           evalExpression id
             (ApplyExpression
                (Apply
                   { function = predicate
                   , argument = arrayItem
                   , location = BuiltIn
                   , typ = typeOutput (expressionType predicate)
                   , style = EvalApply
                   }))
         case bool of
           BoolAtom True -> pure (Just arrayItem)
           BoolAtom False -> pure Nothing
           _ -> do
             RIO.writeSomeRef seenHoleRef True
             pure Nothing)
      expressions
  seenHole <- RIO.readSomeRef seenHoleRef
  pure
    (if seenHole
       then expression
       else ArrayExpression
              Array
                { typ = expressionType expression
                , location = SteppedCursor
                , expressions = (V.mapMaybe id expressions')
                , form = Evaluated
                })

evalMap ::
     Expression Resolved
  -> Expression Resolved
  -> Array Resolved
  -> RIO Eval (Expression Resolved)
evalMap expression func (Array {expressions}) = do
  expressions' <-
    traverse
      (\arrayItem -> do
         evalExpression
           id
           (ApplyExpression
              (Apply
                 { function = func
                 , argument = arrayItem
                 , location = BuiltIn
                 , typ = typeOutput (expressionType func)
                 , style = EvalApply
                 })))
      expressions
  pure
    (ArrayExpression
       Array
         { typ = expressionType expression
         , location = SteppedCursor
         , expressions = expressions'
         , form = Evaluated
         })

evalSort ::
     Expression Resolved
  -> Array Resolved
  -> RIO Eval (Expression Resolved)
evalSort expression list@Array {expressions} = do
  expressions' <- traverse (fmap reify . evalExpression id) (toList expressions)
  pure
    (if any
          (\case
             HoleR {} -> True
             _ -> False)
          expressions'
       then expression
       else ArrayExpression
              list
                { expressions =
                    V.fromList (map originalR (List.sort expressions'))
                , Array.location = SteppedCursor
                , form = Evaluated
                })

evalDistinct ::
     Expression Resolved
  -> Array Resolved
  -> RIO Eval (Expression Resolved)
evalDistinct expression list@Array {expressions} = do
  expressions' <- traverse (fmap reify . evalExpression id) (toList expressions)
  pure
    (if any
          (\case
             HoleR {} -> True
             _ -> False)
          expressions'
       then expression
       else ArrayExpression
              list
                { expressions =
                    V.fromList (map originalR (nubOrd expressions'))
                , Array.location = SteppedCursor
                , form = Evaluated
                })

evalAtomicFold ::
     (Expression Resolved -> Maybe n)
  -> (n -> Expression Resolved)
  -> (Vector n -> n)
  -> Array Resolved
  -> Expression Resolved
  -> Text
  -> RIO Eval (Expression Resolved)
evalAtomicFold reifier reflect reduce (Array {expressions}) expression emptyTag = do
  ns <- traverse (fmap reifier . evalExpression id) expressions
  pure
    (if V.null expressions
       then variantSigged (TagName emptyTag) (expressionType expression) Nothing
       else if any isNothing ns
              then expression
              else variantSigged
                     okTagName
                     (expressionType expression)
                     (pure (reflect (reduce (V.mapMaybe id ns)))))

evalAtomicFoldInteger ::
     (Vector Integer -> Integer)
  -> Array Resolved
  -> Expression Resolved
  -> Text
  -> RIO Eval (Expression Resolved)
evalAtomicFoldInteger =
  evalAtomicFold
    (\case
       IntegerAtom i _ -> pure i
       _ -> Nothing)
    (\i -> IntegerAtom i integerT)

evalAtomicFoldDecimal ::
     Natural
  -> (Vector Decimal -> Decimal)
  -> Array Resolved
  -> Expression Resolved
  -> Text
  -> RIO Eval (Expression Resolved)
evalAtomicFoldDecimal places =
  evalAtomicFold
    (\case
       DecimalAtom i _ -> pure i
       _ -> Nothing)
    (\i -> DecimalAtom i (decimalT places))

evalFind ::
     Expression Resolved
  -> Array Resolved
  -> Expression Resolved
  -> RIO Eval (Expression Resolved)
evalFind predicate (Array {expressions}) expression = do
  seenHoleRef <- RIO.newSomeRef False
  expressions' <-
    traverse
      (\arrayItem -> do
         bool <-
           evalExpression id
             (ApplyExpression
                (Apply
                   { function = predicate
                   , argument = arrayItem
                   , location = BuiltIn
                   , typ = typeOutput (expressionType predicate)
                   , style = EvalApply
                   }))
         case bool of
           BoolAtom True -> pure (Just arrayItem)
           BoolAtom False -> pure Nothing
           _ -> do
             RIO.writeSomeRef seenHoleRef True
             pure Nothing)
      expressions
  seenHole <- RIO.readSomeRef seenHoleRef
  pure
    (if seenHole
       then expression
       else if V.null expressions
              then variantSigged
                     (TagName "find_empty")
                     (expressionType expression)
                     Nothing
              else case V.mapMaybe id expressions' V.!? 0 of
                     Nothing ->
                       variantSigged
                         (TagName "find_failed")
                         (expressionType expression)
                         Nothing
                     Just thing ->
                       variantSigged
                         okTagName
                         (expressionType expression)
                         (pure thing))

evalConcat :: Array Resolved -> Expression Resolved -> RIO Eval (Expression Resolved)
evalConcat Array {expressions, ..} expression = do
  stripped <-
    traverse
      (\case
         ArrayExpression Array{expressions=xs} -> pure (Just xs)
         _ -> pure Nothing)
      expressions
  if V.any isNothing stripped
    then pure expression
    else pure
           (ArrayExpression
              Array
                { location = SteppedCursor
                , form = Evaluated
                , expressions = V.concatMap id (V.mapMaybe id stripped)
                , typ = expressionType expression
                , ..
                })

-- | Just a flipped version of set.
setFlipped :: Is k A_Setter => Optic k is s t a b -> s -> b -> t
setFlipped l v s = set l s v

ignore :: RIO Eval a -> RIO Eval a
ignore = RIO.local (\Eval {..} -> Eval {glogfunc = mempty, ..})
