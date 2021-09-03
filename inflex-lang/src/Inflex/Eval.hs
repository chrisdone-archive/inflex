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
  ) where

import           Data.Bifunctor
import qualified Data.ByteString.Lazy.Builder as SB
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Foldable
import           Data.Functor.Identity
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Vector as V
import           GHC.Natural
import           Inflex.Decimal
import           Inflex.Defaulter
import           Inflex.Derived
import           Inflex.Printer
import           Inflex.Renamer (patternParam)
import           Inflex.Type
import           Inflex.Types
import           Inflex.Types.Eval
import           Inflex.Variants
import           Lexx
import qualified RIO
import           RIO (RIO, glog)


--------------------------------------------------------------------------------
-- Entry points

evalTextRepl :: Bool -> Text -> IO ()
evalTextRepl loud text   = do
  output <-
    RIO.runRIO
      Eval
        { glogfunc =
            if loud
              then RIO.mkGLogFunc (\stack msg -> print stack >> prettyWrite msg)
              else mempty
        , globals = mempty
        }
      (evalTextDefaulted mempty "" text)
  case output of
    Left e -> print (e :: DefaultEvalError ())
    Right e -> do
      putStrLn "=>"
      L8.putStrLn (SB.toLazyByteString (RIO.getUtf8Builder (printer e)))

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
    Right Cell{defaulted} -> fmap Right (evalExpression defaulted)
    Left e -> pure (Left e)

--------------------------------------------------------------------------------
-- Expression evaluator

evalExpression :: Expression Resolved -> RIO Eval (Expression Resolved)
evalExpression expression = do
  glog (EvalStep expression)
  case expression of
    -- Self-evaluating forms:
    LiteralExpression {} -> pure expression
    LambdaExpression {} -> pure expression
    VariableExpression {} -> pure expression
    HoleExpression {} -> pure expression
    -- Data structures:
    RecordExpression record -> evalRecord record
    ArrayExpression array -> evalArray array
    VariantExpression variant -> evalVariant variant
    -- Globals:
    GlobalExpression global -> evalGlobal global
    -- Apply:
    ApplyExpression apply -> evalApply apply
    InfixExpression infix' -> evalInfix infix'
    -- Special forms:
    IfExpression if' -> evalIf if'
    CaseExpression case' -> evalCase case'
    PropExpression prop -> evalProp prop
    -- Disabled forms:
    BoundaryExpression {} -> pure expression
    EarlyExpression {} -> pure expression
    LetExpression {} -> pure expression

--------------------------------------------------------------------------------
-- Case matching

evalCase :: Case Resolved -> RIO Eval (Expression Resolved)
evalCase Case {..} = do
  scrutinee' <- evalExpression scrutinee
  case listToMaybe (mapMaybe (match scrutinee') (toList alternatives)) of
    Just e -> evalExpression e
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

evalProp :: Prop Resolved -> RIO Eval (Expression Resolved)
evalProp prop@Prop {..} = do
  expression' <- evalExpression expression
  case expression' of
    RecordExpression Record {fields} ->
      case find (\FieldE {name = name'} -> name' == name) fields of
        Nothing -> pure expression0
        Just FieldE {expression = v} -> evalExpression v
    _ -> pure expression0
  where
    expression0 = PropExpression prop

evalRecord :: Record Resolved -> RIO Eval (Expression Resolved)
evalRecord Record {..} = do
  fields' <-
    traverse
      (\FieldE {expression, name} -> do
         e' <- evalExpression expression
         pure (FieldE {location = SteppedCursor, expression = e', ..}))
      fields
  pure (RecordExpression (Record {fields = fields', ..}))

evalArray :: Array Resolved -> RIO Eval (Expression Resolved)
evalArray array@Array {..} =
  case form of
    Evaluated -> pure (ArrayExpression array)
    Unevaluated -> do
      expressions' <- traverse evalExpression expressions
      pure
        (ArrayExpression
           Array
             { expressions = expressions'
             , form = Evaluated
             , ..
             })

evalVariant :: Variant Resolved -> RIO Eval (Expression Resolved)
evalVariant Variant {..} = do
  argument' <- traverse evalExpression argument
  pure
    (VariantExpression
       Variant {argument = argument', ..})

--------------------------------------------------------------------------------
-- Globals

evalGlobal :: Global Resolved -> RIO Eval (Expression Resolved)
evalGlobal global@Global {name} = do
  Eval {globals} <- RIO.ask
  case name of
    HashGlobal hash ->
      case M.lookup hash globals of
        Just expression -> evalExpression expression
        Nothing -> do
          glog (GlobalMissing global)
          pure (GlobalExpression global)
    _ -> pure (GlobalExpression global)

--------------------------------------------------------------------------------
-- Apply

-- | Try to apply a lambda, otherwise apply primitive functions.
evalApply :: Apply Resolved -> RIO Eval (Expression Resolved)
evalApply Apply {function, argument, ..} = do
  function' <- evalExpression function
  argument' <- evalExpression argument
  case function' of
    LambdaExpression Lambda {body} -> do
      body' <- betaReduce body argument'
      evalExpression body'
    _ ->
      evalApplyNF
        (ApplyExpression Apply {function = function', argument = argument', ..})

-- | Apply after the function and argument are reduced to normal form.
evalApplyNF :: Expression Resolved -> RIO Eval (Expression Resolved)
evalApplyNF expression =
  case expression of
    ApplyMethod1 FromIntegerGlobal instance' argument@(IntegerAtom integer typ) ->
      evalFromInteger typ argument integer instance'
    ApplyMethod1 FromDecimalGlobal (FromDecimalDecimalInstance instance') (DecimalAtom decimal typ) ->
      evalFromDecimal typ expression decimal instance'
    ApplyFunction2 LengthFunction fromInteger' (ArrayExpression array) ->
      evalLength fromInteger' array (expressionType expression)
    ApplyFunction1 NullFunction argument ->
      apply1 nullFunction argument (expressionType expression)
    _ -> pure expression

--------------------------------------------------------------------------------
-- Infix

evalInfix :: Infix Resolved -> RIO Eval (Expression Resolved)
evalInfix Infix {..} = do
  global' <- evalExpression global
  left' <- evalExpression left
  right' <- evalExpression right
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
    (DecimalAtom (decimalToFixed -> left) typ, DecimalAtom (decimalToFixed -> right) _typ) -> do
      case sameFixed
             left
             right
             (\x y -> do
                result <-
                  case numericBinOp of
                    AddOp -> pure (x + y)
                    SubtractOp -> pure (x - y)
                    MulitplyOp -> pure (x * y)
                    DivideOp ->
                      if y == 0
                        then Left () -- We stop due to division by zero.
                        else pure (x / y)
                pure (fixedToDecimal (SomeFixed places result))) of
        Nothing -> pure expression
        Just (Left ()) -> pure expression -- Division by zero has no answer, so we stop.
        Just (Right result) ->
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

evalIf :: If Resolved -> RIO Eval (Expression Resolved)
evalIf If {..} = do
  condition' <- evalExpression condition
  case condition of
    BoolAtom bool ->
      if bool
        then evalExpression consequent
        else evalExpression alternative
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
  evalApply
    Apply
      { location = BuiltIn
      , function
      , argument
      , typ
      , style = EvalApply
      }

apply2 ::
     Expression Resolved
  -> Expression Resolved
  -> Expression Resolved
  -> Type Generalised
  -> RIO Eval (Expression Resolved)
apply2 function argument1 argument2 typ =
  evalApply
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
      , argument =
          argument2
      , style = EvalApply
      }

--------------------------------------------------------------------------------
-- Reification/reflection

pattern BoolAtom :: Bool -> Expression Resolved
pattern BoolAtom bool <-
  VariantExpression Variant {tag = TagName ((== "true") -> bool), argument = Nothing}
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
