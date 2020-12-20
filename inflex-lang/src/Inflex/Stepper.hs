{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Step the code.

module Inflex.Stepper where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Bifunctor
import           Data.Foldable
import           Data.Functor.Identity
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Vector as V
import           Inflex.Decimal
import           Inflex.Defaulter
import           Inflex.Derived
import           Inflex.Renamer (patternParam)
import           Inflex.Resolver
import           Inflex.Type
import           Inflex.Types
import           Inflex.Types as Apply (Apply(..))
import           Inflex.Variants
import           Numeric.Natural
import qualified RIO
import           RIO (RIO)

--------------------------------------------------------------------------------
-- Types

data ResolveStepError e
  = ResolverErrored (GeneraliseResolveError e)
  | StepError StepError
  deriving (Show, Eq)

data DefaultStepError e
  = DefaulterErrored (ResolverDefaulterError e)
  | StepError' StepError
  deriving (Show, Eq)

data StepError
  = NotInScope Hash
  | InvalidIntegerOpOperands (Expression Resolved) (Expression Resolved)
  | InvalidDecimalOpOperands (Expression Resolved) (Expression Resolved)
  | MismatchingPrecisionsInOp
  | MismatchingPrecisionsInFromDecimal Natural Natural
  | CannotShrinkADecimalFromTo Natural Natural
  | NotARecord
  | MissingRecordKey FieldName
  deriving (Show, Eq)

data Stepped
  = Continue
  | Stepped

newtype Step e a = Step
  { unStep :: ReaderT (Map Hash (Expression Resolved)) (StateT Stepped (Either StepError)) a
  } deriving ( Functor
             , Monad
             , Applicative
             , MonadState Stepped
             , MonadReader (Map Hash (Expression Resolved))
             )

--------------------------------------------------------------------------------
-- Main entry points

data StepReader = StepReader

-- TODO: Add a configuration with limits: number of steps, memory used, etc.

stepText ::
     Map Hash (Either e (Scheme Polymorphic))
  -> Map Hash (Expression Resolved)
  -> FilePath
  -> Text
  -> RIO StepReader (Either (ResolveStepError e) (Expression Resolved))
stepText schemes values fp text = do
  result <-
    RIO.runRIO
      ResolveReader
      (fmap (first ResolverErrored) (resolveText schemes fp text))
  case result of
    Left e -> pure (Left e)
    Right IsResolved {thing} ->
      pure
        (first
           StepError
           (evalStateT
              (runReaderT (unStep (stepExpression thing)) values)
              Continue))

stepTextDefaulted ::
     Map Hash (Either e (Scheme Polymorphic))
  -> Map Hash (Expression Resolved)
  -> FilePath
  -> Text
  -> RIO StepReader (Either (DefaultStepError e) (Expression Resolved))
stepTextDefaulted schemes values fp text = do
  cell <-
    RIO.runRIO
      DefaulterReader
      (fmap (first DefaulterErrored) (defaultText schemes fp text))
  case cell of
    Left e -> pure (Left e)
    Right cell' -> stepDefaulted values cell'

stepDefaulted ::
     Map Hash (Expression Resolved)
  -> Cell
  -> RIO StepReader (Either (DefaultStepError e) (Expression Resolved))
stepDefaulted values Cell{defaulted} = pure (do
   first
     StepError'
     (evalStateT
        (runReaderT (unStep (stepExpression defaulted)) values)
        Continue))

stepExpression ::
     Expression Resolved
  -> Step e (Expression Resolved)
stepExpression expression = do
  stepped <- get
  case stepped of
    Stepped -> pure expression
    Continue ->
      case expression of
        ApplyExpression apply -> stepApply apply
        PropExpression prop -> stepProp prop
        ArrayExpression array -> stepArray array
        VariantExpression variant -> stepVariant variant
        RecordExpression record -> stepRecord record
        InfixExpression infix' -> stepInfix infix'
        IfExpression if' -> stepIf if'
        CaseExpression case' -> stepCase case'
        GlobalExpression global -> stepGlobal global
        LiteralExpression {} -> pure expression
        LambdaExpression {} -> pure expression
        VariableExpression {} -> pure expression
        HoleExpression {} -> pure expression
        LetExpression {} -> pure expression

--------------------------------------------------------------------------------
-- Records

stepRecord :: Record Resolved -> Step e (Expression Resolved)
stepRecord Record {..} = do
  fields' <-
    traverse
      (\FieldE {location = l, ..} -> do
         e' <- stepExpression expression
         pure FieldE {location = l, expression = e', ..})
      fields
  pure (RecordExpression (Record {fields = fields', ..}))

stepProp :: Prop Resolved -> Step e (Expression Resolved)
stepProp Prop {..} = do
  expression' <- stepExpression expression
  stepped <- get
  case stepped of
    Stepped -> pure (PropExpression Prop {expression = expression', ..})
    Continue ->
      case expression' of
        RecordExpression Record {fields} ->
          case find (\FieldE {name = name'} -> name' == name) fields of
            Nothing -> Step (lift (lift (Left (MissingRecordKey name))))
            Just FieldE{expression = v} -> stepExpression v
        _ -> Step (lift (lift (Left NotARecord)))

stepArray :: Array Resolved -> Step e (Expression Resolved)
stepArray Array {..} = do
  expressions' <- traverse stepExpression expressions
  pure (ArrayExpression Array {expressions = expressions', ..})

stepVariant :: Variant Resolved -> Step e (Expression Resolved)
stepVariant Variant {..} = do
  argument' <- traverse stepExpression argument
  pure (VariantExpression Variant {argument = argument', ..})

stepIf :: If Resolved -> Step e (Expression Resolved)
stepIf If {..} = do
  condition' <- stepExpression condition
  case reifyBool condition' of
    Nothing -> pure (IfExpression If {condition = condition', ..})
    Just True -> stepExpression consequent
    Just False -> stepExpression alternative

stepCase :: Case Resolved -> Step e (Expression Resolved)
stepCase Case {..} = do
  scrutinee' <- stepExpression scrutinee
  case listToMaybe (mapMaybe (match scrutinee') (toList alternatives)) of
    Just e -> stepExpression e
    -- TODO: warn about unmatched case?
    Nothing -> pure (CaseExpression (Case {scrutinee = scrutinee', ..}))

-- | Finds a match, if any, and the result must be stepped again.
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
-- Globals

stepGlobal :: Global Resolved -> Step e (Expression Resolved)
stepGlobal global@Global {name} = do
  hashes <- ask
  case name of
    HashGlobal hash ->
      case M.lookup hash hashes of
        Just expre -> pure expre
        Nothing -> Step (lift (lift (Left (NotInScope hash))))
    _ -> pure (GlobalExpression global)

--------------------------------------------------------------------------------
-- Function application

stepApply :: Apply Resolved -> Step e (Expression Resolved)
stepApply Apply {..} = do
  function' <- stepExpression function
  argument' <- stepExpression argument
  stepped <- get
  case stepped of
    Stepped ->
      pure
        (ApplyExpression Apply {function = function', argument = argument', ..})
    Continue -> do
      case function' of
        LambdaExpression Lambda{body} -> do
          body' <- betaReduce body argument'
          stepExpression body'
        GlobalExpression (Global {name = FunctionGlobal func}) | elem func [LengthFunction,NullFunction] ->
          stepFunction1 typ func argument'
        ApplyExpression Apply { function = GlobalExpression (Global {name = FromIntegerGlobal})
                              , argument = GlobalExpression (Global {name = (InstanceGlobal FromIntegerIntegerInstance)})
                              }
          | LiteralExpression (NumberLiteral (Number {number = IntegerNumber {}})) <-
             argument' -> pure argument'
        ApplyExpression Apply { function = GlobalExpression (Global {name = FromDecimalGlobal})
                              , argument = GlobalExpression (Global {name = (InstanceGlobal (FromDecimalDecimalInstance fromDecimalInstance))})
                              }
          | LiteralExpression (NumberLiteral (Number { number = DecimalNumber decimal
                                                     , typ = typ'
                                                     })) <- argument' -> do
            decimal' <- fromDecimalStep fromDecimalInstance decimal
            pure
              (LiteralExpression
                 (NumberLiteral
                    (Number {number = DecimalNumber decimal', typ = typ', ..})))
        ApplyExpression Apply { function = GlobalExpression (Global {name = FromIntegerGlobal})
                              , argument = GlobalExpression (Global {name = (InstanceGlobal (FromIntegerDecimalInstance supersetPlaces))})
                              }
          | LiteralExpression (NumberLiteral (Number { number = IntegerNumber integer
                                                     , typ = typ'
                                                     })) <- argument' -> do
            pure
              (LiteralExpression
                 (NumberLiteral
                    (Number
                       { number =
                           DecimalNumber
                             (decimalFromInteger integer supersetPlaces)
                       , typ = typ'
                       , ..
                       })))
        ApplyExpression Apply { function = GlobalExpression Global {name = FunctionGlobal functionName}
                              , argument = functionExpression
                              , location = applyLocation
                              } ->
          stepFunction2
            functionName
            argument'
            functionExpression
            location
            applyLocation
            typ
        _ ->
          pure
            (ApplyExpression
               Apply {function = function', argument = argument', ..})

--------------------------------------------------------------------------------
-- Function stepper

stepFunction1 ::
     Type Generalised
  -> Function
  -> Expression Resolved
  -> Step e (Expression Resolved)
stepFunction1 returnType func argument =
  case func of
    LengthFunction ->
      case argument of
        ArrayExpression Array {expressions} -> do
          pure
            (LiteralExpression
               (NumberLiteral
                  Number
                    { number =
                        IntegerNumber (fromIntegral (V.length expressions))
                    , location = SteppedCursor
                    , typ = returnType
                    }))
        _ -> error "Invalid argument to function."
    NullFunction ->
      do result <- stepApply
           Apply
             { location = BuiltIn
             , function = nullFunction
             , argument
             , typ = returnType
             }
         pure result
    _ -> error "bad 1-ary function."

stepFunction2 ::
     Function
  -> Expression Resolved
  -> Expression Resolved
  -> Cursor
  -> Cursor
  -> Type Generalised
  -> Step e (Expression Resolved)
stepFunction2 function argument' functionExpression location applyLocation originalArrayType =
  case function of
    MapFunction ->
      case argument' of
        ArrayExpression Array {expressions} -> do
          expressions' <-
            traverse
              (\arrayItem ->
                 stepExpression
                   (ApplyExpression
                      (Apply
                         { function = functionExpression
                         , argument = arrayItem
                         , location = location
                         , typ = typeOutput (expressionType functionExpression)
                         })))
              expressions
          stepped' <- get
          case stepped' of
            Stepped -> error "TODO: stepped form."
            Continue ->
              pure
                (ArrayExpression
                   Array
                     { typ =
                         ArrayType
                           (typeOutput (expressionType functionExpression))
                     , location = applyLocation
                     , expressions = expressions'
                     })
        _ -> error "Invalid argument to function."
    FilterFunction ->
      case argument' of
        ArrayExpression Array {expressions} -> do
          expressions' <-
            traverse
              (\arrayItem -> do
                 arrayItem' <- stepExpression arrayItem
                 bool <-
                   stepExpression
                     (ApplyExpression
                        (Apply
                           { function = functionExpression
                           , argument = arrayItem'
                           , location = location
                           , typ =
                               typeOutput (expressionType functionExpression)
                           }))
                 case reifyBool bool of
                   Just True -> pure (Just arrayItem')
                   Just False -> pure Nothing
                   Nothing -> pure (Just (holeExpression (expressionType arrayItem))))
              expressions
          stepped' <- get
          case stepped' of
            Stepped -> error "TODO: stepped form."
            Continue ->
              pure
                (ArrayExpression
                   Array
                     { typ = originalArrayType
                     , location = applyLocation
                     , expressions = (V.mapMaybe id expressions')
                     })
        _ -> error "Invalid argument to function."
    _ -> error "bad arity"

--------------------------------------------------------------------------------
-- Infix stepper

stepInfix :: Infix Resolved -> Step e (Expression Resolved)
stepInfix Infix {..} = do
  global' <- stepExpression global
  left' <- stepExpression left
  right' <- stepExpression right
  stepped <- get
  let asis =
        (InfixExpression
           Infix {global = global', left = left', right = right', ..})
  case stepped of
    Stepped -> pure asis
    Continue ->
      case (left', right') of
        (HoleExpression {}, _) -> pure asis
        (_, HoleExpression {}) -> pure asis
        _ ->
          case global' of
            -- Arithmetic
            ApplyExpression Apply { function = GlobalExpression Global {name = NumericBinOpGlobal {}}
                                  , argument = GlobalExpression Global {name = InstanceGlobal (IntegerOpInstance numericBinOp)}
                                  } ->
              stepIntegerOp asis numericBinOp left' right'
            ApplyExpression Apply { function = GlobalExpression Global {name = NumericBinOpGlobal {}}
                                  , argument = GlobalExpression Global {name = InstanceGlobal (DecimalOpInstance precision numericBinOp)}
                                  } ->
              stepDecimalOp asis precision numericBinOp left' right'
            -- Equality
            ApplyExpression Apply { function = GlobalExpression Global {name = EqualGlobal equality}
                                  , argument = GlobalExpression Global {name = InstanceGlobal EqualIntegerInstance}
                                  , location = location'
                                  } ->
              stepAtomicEquality asis location' equality left' right'
            ApplyExpression Apply { function = GlobalExpression Global {name = EqualGlobal equality}
                                  , argument = GlobalExpression Global {name = InstanceGlobal EqualTextInstance}
                                  , location = location'
                                  } ->
              stepAtomicEquality asis location' equality left' right'
            ApplyExpression Apply { function = GlobalExpression Global {name = EqualGlobal equality}
                                  , argument = GlobalExpression Global {name = InstanceGlobal EqualDecimalInstance {}}
                                  , location = location'
                                  } ->
              stepAtomicEquality asis location' equality left' right'
            -- Compareity
            ApplyExpression Apply { function = GlobalExpression Global {name = CompareGlobal compareity}
                                  , argument = GlobalExpression Global {name = InstanceGlobal CompareIntegerInstance}
                                  , location = location'
                                  } ->
              stepAtomicComparison asis location' compareity left' right'
            ApplyExpression Apply { function = GlobalExpression Global {name = CompareGlobal compareity}
                                  , argument = GlobalExpression Global {name = InstanceGlobal CompareTextInstance}
                                  , location = location'
                                  } ->
              stepAtomicComparison asis location' compareity left' right'
            ApplyExpression Apply { function = GlobalExpression Global {name = CompareGlobal compareity}
                                  , argument = GlobalExpression Global {name = InstanceGlobal CompareDecimalInstance {}}
                                  , location = location'
                                  } ->
              stepAtomicComparison asis location' compareity left' right'
            _ -> error ("stepInfix: " ++ show global')

--------------------------------------------------------------------------------
-- Equality

stepAtomicEquality ::
     Applicative f
  => Expression Resolved
  -> Cursor
  -> Equality
  -> Expression Resolved
  -> Expression Resolved
  -> f (Expression Resolved)
stepAtomicEquality asis location equality left' right' =
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
    _ -> pure asis
  where
    comparator :: Eq a => a -> a -> Bool
    comparator =
      case equality of
        Equal -> (==)
        NotEqual -> (/=)

--------------------------------------------------------------------------------
-- Compareity

stepAtomicComparison ::
     Applicative f
  => Expression Resolved
  -> Cursor
  -> Comparison
  -> Expression s1
  -> Expression s2
  -> f (Expression Resolved)
stepAtomicComparison asis location compareity left' right' =
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
    _ -> pure asis
  where
    comparator :: Ord a => a -> a -> Bool
    comparator =
      case compareity of
        GreaterThan -> (>)
        LessThan -> (<)
        GreaterEqualTo -> (>=)
        LessEqualTo -> (<=)

--------------------------------------------------------------------------------
-- Numeric operations

stepIntegerOp ::
     Expression Resolved
  -> NumericBinOp
  -> Expression Resolved
  -> Expression Resolved
  -> Step e (Expression Resolved)
stepIntegerOp asis numericBinOp left' right' =
  case (left', right') of
    (LiteralExpression (NumberLiteral Number {number = IntegerNumber left, typ}), LiteralExpression (NumberLiteral Number {number = IntegerNumber right}))
      | DivideOp <- numericBinOp, 0 <- right -> pure asis -- Nothing to do for division by zero.
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
    _ {-Step (lift (lift (Left (InvalidIntegerOpOperands left' right'))))-}
     -- warn
     -> pure asis

stepDecimalOp ::
     Expression Resolved
  -> Natural
  -> NumericBinOp
  -> Expression Resolved
  -> Expression Resolved
  -> Step e (Expression Resolved)
stepDecimalOp asis places numericBinOp left' right' =
  case (left', right') of
    (LiteralExpression (NumberLiteral Number { number = DecimalNumber (decimalToFixed -> left)
                                             , typ
                                             }), LiteralExpression (NumberLiteral Number {number = DecimalNumber (decimalToFixed -> right)})) -> do
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
        Nothing -> Step (lift (lift (Left MismatchingPrecisionsInOp)))
        Just (Left ()) -> pure asis -- Division by zero has no answer, so we stop.
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
    -- _ -> Step (lift (lift (Left (InvalidDecimalOpOperands left' right'))))
    _ -> pure asis

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
-- FromDecimal instance stepping

fromDecimalStep :: FromDecimalInstance -> Decimal -> Step e Decimal
fromDecimalStep fromDecimalInstance decimal =
  if thisSubsetPlaces == subsetPlaces
    then if thisSubsetPlaces == supersetPlaces
           then pure decimal
           else if thisSubsetPlaces < supersetPlaces
                  then pure (expandDecimalPrecision supersetPlaces decimal)
                  else Step
                         (lift
                            (lift
                               (Left
                                  (CannotShrinkADecimalFromTo
                                     thisSubsetPlaces
                                     supersetPlaces))))
    else Step
           (lift
              (lift
                 (Left
                    (MismatchingPrecisionsInFromDecimal
                       thisSubsetPlaces
                       subsetPlaces))))
  where
    Decimal {places = thisSubsetPlaces} = decimal
    FromDecimalInstance {supersetPlaces, subsetPlaces} = fromDecimalInstance

--------------------------------------------------------------------------------
-- Helpers

holeExpression :: (StagedLocation s ~ Cursor) => StagedType s -> Expression s
holeExpression typ = HoleExpression Hole {location = BuiltIn, typ}
