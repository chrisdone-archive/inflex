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
  | InvalidDecimalOpOperands
  | MismatchingPrecisions
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

--------------------------------------------------------------------------------
-- Steppers

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
        LambdaExpression lambda -> error "OK, lambda!"
        ApplyExpression Apply { function = GlobalExpression (Global {name = FromIntegerGlobal})
                              , argument = GlobalExpression (Global {name = (InstanceGlobal FromIntegerIntegerInstance)})
                              }
          | LiteralExpression (NumberLiteral (Number {number = IntegerNumber {}})) <-
             argument' -> pure argument'
        _ ->
          pure
            (ApplyExpression
               Apply {function = function', argument = argument', ..})


betaReduce :: Lambda Resolved -> Expression Resolved -> Expression Resolved
betaReduce Lambda {body} arg = go 0 body
  where go depth = undefined

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
        Nothing -> Step (lift (lift (Left MismatchingPrecisions)))
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
    _ -> Step (lift (lift (Left InvalidDecimalOpOperands)))
