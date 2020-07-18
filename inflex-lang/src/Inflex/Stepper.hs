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
import Inflex.Resolver
import Inflex.Types

--------------------------------------------------------------------------------
-- Types

data ResolveStepError
  = ResolverErrored GeneraliseResolveError
  | StepError StepError
  deriving (Show, Eq)

data StepError
  = NotInScope Hash
  | InvalidPrimOpOperands
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
    Continue ->
      pure
        (ApplyExpression Apply {function = function', argument = argument', ..})

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
    _ -> Step (lift (lift (Left InvalidPrimOpOperands)))
