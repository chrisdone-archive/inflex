{-# LANGUAGE ViewPatterns #-}
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
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import           Inflex.Decimal
import           Inflex.Defaulter
import           Inflex.Resolver
import           Inflex.Type
import           Inflex.Types
import           Inflex.Types as Apply (Apply(..))
import           Numeric.Natural

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

-- TODO: Add a configuration with limits: number of steps, memory used, etc.

stepText ::
     Map Hash (Either e (Scheme Polymorphic))
  -> Map Hash (Expression Resolved)
  -> FilePath
  -> Text
  -> Either (ResolveStepError e) (Expression Resolved)
stepText schemes values fp text = do
  IsResolved {thing} <- first ResolverErrored (resolveText schemes fp text)
  first
    StepError
    (evalStateT
       (runReaderT (unStep (stepExpression thing)) values)
       Continue)

stepTextDefaulted ::
     Map Hash (Either e (Scheme Polymorphic))
  -> Map Hash (Expression Resolved)
  -> FilePath
  -> Text
  -> Either (DefaultStepError e) (Expression Resolved)
stepTextDefaulted schemes values fp text = do
  cell <- first DefaulterErrored (defaultText schemes fp text)
  stepDefaulted values cell

stepDefaulted ::
     Map Hash (Expression Resolved)
  -> Cell
  -> Either (DefaultStepError e) (Expression Resolved)
stepDefaulted values Cell{defaulted} = do
  first
    StepError'
    (evalStateT
       (runReaderT (unStep (stepExpression defaulted)) values)
       Continue)

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
        RecordExpression record -> stepRecord record
        InfixExpression infix' -> stepInfix infix'
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
        ApplyExpression Apply { function = GlobalExpression Global {name = FunctionGlobal MapFunction}
                                    , argument = functionExpression
                                    , location = applyLocation
                                    } ->
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
                Stepped ->
                  error "TODO: stepped form."
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
            _ ->
              pure
                (ApplyExpression
                   Apply {function = function', argument = argument', ..} -- TODO: error here.
                 )
        _ ->
          pure
            (ApplyExpression
               Apply {function = function', argument = argument', ..})

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
            ApplyExpression Apply { function = GlobalExpression Global {name = NumericBinOpGlobal {}}
                                  , argument = GlobalExpression Global {name = InstanceGlobal (IntegerOpInstance numericBinOp)}
                                  } -> stepIntegerOp asis numericBinOp left' right'
            ApplyExpression Apply { function = GlobalExpression Global {name = NumericBinOpGlobal {}}
                                  , argument = GlobalExpression Global {name = InstanceGlobal (DecimalOpInstance precision numericBinOp)}
                                  } ->
              stepDecimalOp asis precision numericBinOp left' right'
            _ -> error ("stepInfix: " ++ show global')

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
    _ -> {-Step (lift (lift (Left (InvalidIntegerOpOperands left' right'))))-} -- warn
      pure asis

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
    -- _ -> Step (lift (lift (Left (InvalidDecimalOpOperands left' right'))))
    _ -> pure asis

--------------------------------------------------------------------------------
-- Beta reduction

betaReduce ::
     Lambda Resolved -> Expression Resolved -> Step e (Expression Resolved)
betaReduce Lambda {body = body0} arg = go 0 body0
  where
    go :: DeBrujinNesting -> Expression Resolved -> Step e (Expression Resolved)
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
        PropExpression Prop {..} -> do
          expression' <- go deBrujinNesting expression
          pure (PropExpression Prop {expression = expression', ..})
        ArrayExpression Array {..} -> do
          expressions' <- traverse (go deBrujinNesting) expressions
          pure (ArrayExpression Array {expressions = expressions', ..})
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
