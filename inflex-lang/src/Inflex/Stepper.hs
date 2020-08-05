{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Step the code.

module Inflex.Stepper where

import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor
import Data.Map.Strict (Map)
import Data.Text (Text)
import Inflex.Decimal
import Inflex.Defaulter
import Inflex.Resolver
import Inflex.Types
import Numeric.Natural

--------------------------------------------------------------------------------
-- Types

data ResolveStepError
  = ResolverErrored GeneraliseResolveError
  | StepError StepError
  deriving (Show, Eq)

data DefaultStepError
  = DefaulterErrored ResolverDefaulterError
  | StepError' StepError
  deriving (Show, Eq)

data StepError
  = NotInScope Hash
  | InvalidIntegerOpOperands (Expression Resolved) (Expression Resolved)
  | InvalidDecimalOpOperands (Expression Resolved) (Expression Resolved)
  | MismatchingPrecisionsInOp
  | MismatchingPrecisionsInFromDecimal Natural Natural
  | CannotShrinkADecimalFromTo Natural Natural
  deriving (Show, Eq)

data Stepped
  = Continue
  | Stepped

newtype Step a = Step
  { unStep :: ReaderT (Map Hash (Expression Resolved)) (StateT Stepped (Either StepError)) a
  } deriving ( Functor
             , Monad
             , Applicative
             , MonadState Stepped
             , MonadReader (Map Hash (Expression Resolved))
             )

--------------------------------------------------------------------------------
-- Main entry points

stepText ::
     Map Hash (Scheme Polymorphic)
  -> Map Hash (Expression Resolved)
  -> FilePath
  -> Text
  -> Either ResolveStepError (Expression Resolved)
stepText schemes values fp text = do
  IsResolved {thing} <- first ResolverErrored (resolveText schemes fp text)
  first
    StepError
    (evalStateT
       (runReaderT (unStep (stepExpression thing)) values)
       Continue)

stepTextDefaulted ::
     Map Hash (Scheme Polymorphic)
  -> Map Hash (Expression Resolved)
  -> FilePath
  -> Text
  -> Either DefaultStepError (Expression Resolved)
stepTextDefaulted schemes values fp text = do
  Cell{expression} <- first DefaulterErrored (defaultText schemes fp text)
  first
    StepError'
    (evalStateT
       (runReaderT (unStep (stepExpression expression)) values)
       Continue)

stepExpression ::
     Expression Resolved
  -> Step (Expression Resolved)
stepExpression expression = do
  stepped <- get
  case stepped of
    Stepped -> pure expression
    Continue ->
      case expression of
        ApplyExpression apply -> stepApply apply
        InfixExpression infix' -> stepInfix infix'
        LiteralExpression {} -> pure expression
        LambdaExpression {} -> pure expression
        VariableExpression {} -> pure expression
        GlobalExpression {} -> pure expression
        LetExpression {} -> pure expression

--------------------------------------------------------------------------------
-- Function application

stepApply :: Apply Resolved -> Step (Expression Resolved)
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
        LambdaExpression lambda -> do
          body' <- betaReduce lambda argument'
          stepExpression body'
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
        _ ->
          pure
            (ApplyExpression
               Apply {function = function', argument = argument', ..})

--------------------------------------------------------------------------------
-- Infix stepper

stepInfix :: Infix Resolved -> Step (Expression Resolved)
stepInfix Infix {..} = do
  global' <- stepExpression global
  left' <- stepExpression left
  right' <- stepExpression right
  stepped <- get
  case stepped of
    Stepped ->
      pure
        (InfixExpression
           Infix {global = global', left = left', right = right', ..})
    Continue ->
      case global' of
        ApplyExpression Apply { function = GlobalExpression Global {name = NumericBinOpGlobal {}}
                              , argument = GlobalExpression Global {name = InstanceGlobal (IntegerOpInstance numericBinOp)}
                              } -> stepIntegerOp numericBinOp left' right'
        ApplyExpression Apply { function = GlobalExpression Global {name = NumericBinOpGlobal {}}
                              , argument = GlobalExpression Global {name = InstanceGlobal (DecimalOpInstance precision numericBinOp)}
                              } -> stepDecimalOp precision numericBinOp left' right'
        _ -> error ("stepInfix: " ++ show global')

--------------------------------------------------------------------------------
-- Numeric operations

stepIntegerOp ::
     NumericBinOp
  -> Expression Resolved
  -> Expression Resolved
  -> Step (Expression Resolved)
stepIntegerOp numericBinOp left' right' =
  case (left', right') of
    (LiteralExpression (NumberLiteral Number {number = IntegerNumber left, typ}), LiteralExpression (NumberLiteral Number {number = IntegerNumber right})) -> do
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
    _ -> Step (lift (lift (Left (InvalidIntegerOpOperands left' right'))))

stepDecimalOp ::
     Natural
  -> NumericBinOp
  -> Expression Resolved
  -> Expression Resolved
  -> Step (Expression Resolved)
stepDecimalOp places numericBinOp left' right' =
  case (left', right') of
    (LiteralExpression (NumberLiteral Number { number = DecimalNumber (decimalToFixed -> left)
                                             , typ
                                             }), LiteralExpression (NumberLiteral Number {number = DecimalNumber (decimalToFixed -> right)})) -> do
      case sameFixed
             left
             right
             (\x y ->
                fixedToDecimal
                  (SomeFixed
                     places
                     (case numericBinOp of
                        AddOp -> x + y
                        SubtractOp -> x - y
                        MulitplyOp -> x * y
                        -- TODO: Catch x/0 and stop the
                        -- evaluator. Possibly later, we could print
                        -- the origin of the 0. See SteppedCursor
                        -- below.
                        DivideOp -> x / y))) of
        Nothing -> Step (lift (lift (Left MismatchingPrecisionsInOp)))
        Just result ->
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
    _ -> Step (lift (lift (Left (InvalidDecimalOpOperands left' right'))))

--------------------------------------------------------------------------------
-- Beta reduction

betaReduce ::
     Lambda Resolved -> Expression Resolved -> Step (Expression Resolved)
betaReduce Lambda {body = body0} arg = go 0 body0
  where
    go :: DeBrujinNesting -> Expression Resolved -> Step (Expression Resolved)
    go deBrujinNesting =
      \case
        e@(VariableExpression Variable {name})
          | deBrujinIndexNesting name == deBrujinNesting -> pure arg
          | otherwise -> pure e
        LambdaExpression Lambda {..} -> do
          body' <- go (deBrujinNesting + 1) body
          pure (LambdaExpression Lambda {body = body', ..})
        ApplyExpression Apply {..} -> do
          argument' <- go deBrujinNesting argument
          function' <- go deBrujinNesting function
          pure
            (ApplyExpression
               Apply {argument = argument', function = function', ..})
        InfixExpression Infix{..} -> do
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

--------------------------------------------------------------------------------
-- FromDecimal instance stepping

fromDecimalStep :: FromDecimalInstance -> Decimal -> Step Decimal
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