{-# OPTIONS -F -pgmF=early #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
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
import           Data.Bifunctor
import qualified Data.ByteString.Lazy.Builder as SB
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Containers.ListUtils
import           Data.Early
import qualified Control.Early (Early(..), early)
import           Data.Foldable
import           Data.Functor.Identity
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Vector as V
import           Inflex.Decimal
import           Inflex.Defaulter
import           Inflex.Derived
import           Inflex.Printer
import           Inflex.Renamer (patternParam)
import           Inflex.Resolver
import           Inflex.Type
import           Inflex.Types
import           Inflex.Types as Apply (Apply(..))
import           Inflex.Types as Array (Array(..))
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
  | EarlyReturnWithoutBoundary (Expression Resolved)
  | InvalidReifyValue (Expression Resolved)
  | ShouldGetBool
  | Didn'tExpectFoundHoleHere
  | NeededArrayForConcat
  deriving (Show, Eq)

data Result e
  = Returned (Expression Resolved)
  | FoundHole (Expression Resolved)
  | Errored StepError
  | Ok e
  deriving (Functor)

instance Applicative Result where
  pure = Ok
  (<*>) = error "This isn't necessary."

instance Control.Early.Early Result where
  dispatch r f =
    case r of
      Returned e -> pure (Returned e)
      Errored e -> pure (Errored e)
      FoundHole e -> pure (FoundHole e)
      Ok x -> f x

newtype Step a = Step
  { unStep :: Reader (Map Hash (Expression Resolved)) a
  } deriving ( Functor
             , Monad
             , Applicative
             , MonadReader (Map Hash (Expression Resolved))
             )

--------------------------------------------------------------------------------
-- Main entry points

data StepReader = StepReader

-- TODO: Add a configuration with limits: number of steps, memory used, etc.

stepTextRepl :: Text -> IO ()
stepTextRepl text = do
  output <- RIO.runRIO StepReader (stepTextDefaulted mempty mempty "" text)
  case output of
    Left e -> print (e :: DefaultStepError ())
    Right e -> do
      putStrLn "=>"
      L8.putStrLn (SB.toLazyByteString (RIO.getUtf8Builder (printer e)))

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
      pure (first StepError (resultToEither (runReader (unStep (stepExpression thing)) values)))

resultToEither ::
     Result (Expression Resolved) -> Either StepError (Expression Resolved)
resultToEither =
  \case
    Ok e -> pure e
    Errored e -> Left e
    FoundHole e -> pure e
    Returned e -> Left (EarlyReturnWithoutBoundary e)

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
stepDefaulted values Cell {defaulted} =
  pure
    (do first
          StepError'
          (resultToEither (runReader (unStep (stepExpression defaulted)) values)))

stepExpression ::
     Expression Resolved
  -> Step (Result (Expression Resolved))
stepExpression expression = do
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
    LiteralExpression {} -> pure (Ok expression)
    LambdaExpression {} -> pure (Ok expression)
    VariableExpression {} -> pure (Ok expression)
    HoleExpression {} -> pure (Ok expression)
    LetExpression {} -> pure (Ok expression)
    EarlyExpression early -> stepEarly early
    BoundaryExpression boundary -> stepBoundary boundary

--------------------------------------------------------------------------------
-- Early return

stepEarly :: Early Resolved -> Step (Result (Expression Resolved))
stepEarly Early {expression} = do
  tag <- stepExpression expression?
  case tag of
    VariantExpression Variant {tag = TagName "ok", argument = Just v} -> pure (Ok v)
    _ -> pure (Returned tag)

stepBoundary :: Boundary Resolved -> Step (Result (Expression Resolved))
stepBoundary Boundary {expression, typ} = do
  result <- stepExpression expression
  case result of
    Returned e -> pure (Ok e)
    Ok e -> pure (Ok (variantSigged okTagName typ (pure e)))
    FoundHole{} -> pure (Errored Didn'tExpectFoundHoleHere)
    Errored e -> pure (Errored e)

--------------------------------------------------------------------------------
-- Records

stepRecord :: Record Resolved -> Step (Result (Expression Resolved))
stepRecord Record {..} = do
  fields' <-
    traverseE
      (\FieldE {expression, name} -> do
         e' <- stepExpression expression?
         pure (Ok (FieldE {location = SteppedCursor, expression = e', ..})))
      fields?
  pure (Ok (RecordExpression (Record {fields = fields', ..})))

stepProp :: Prop Resolved -> Step (Result (Expression Resolved))
stepProp Prop {..} = do
  expression' <- stepExpression expression?
  case expression' of
    RecordExpression Record {fields} ->
      case find (\FieldE {name = name'} -> name' == name) fields of
        Nothing -> pure (Errored (MissingRecordKey name))
        Just FieldE{expression = v} -> stepExpression v
    _ -> pure (Errored NotARecord)

stepArray :: Array Resolved -> Step (Result (Expression Resolved))
stepArray Array {..} =
  case form of
    Evaluated -> pure (Ok (ArrayExpression (Array {..})))
    Unevaluated -> do
      expressions' <- traverseE stepExpression expressions?
      pure
        (Ok
           (ArrayExpression
              Array
                { expressions = expressions'
                , form = Evaluated
                , ..
                }))

stepVariant :: Variant Resolved -> Step (Result (Expression Resolved))
stepVariant Variant {..} = do
  argument' <- traverseE stepExpression argument?
  pure
    (Ok
       (VariantExpression
          Variant {argument = argument', location = SteppedCursor, ..}))

stepIf :: If Resolved -> Step (Result (Expression Resolved))
stepIf If {..} = do
  condition' <- stepExpression condition?
  boolR <- reify condition'
  case boolR of
    Ok (BoolR _ True) -> stepExpression consequent
    Ok (BoolR _ False) -> stepExpression alternative
    Ok _ -> pure (Errored ShouldGetBool)
    Returned r -> pure (Returned r)
    FoundHole {} ->
      pure
        (Ok
           (IfExpression
              If {condition = condition', location = SteppedCursor, ..}))
    Errored e -> pure (Errored e)

stepCase :: Case Resolved -> Step (Result (Expression Resolved))
stepCase Case {..} = do
  scrutinee' <- stepExpression scrutinee?
  case listToMaybe (mapMaybe (match scrutinee') (toList alternatives)) of
    Just e -> stepExpression e
    -- TODO: warn about unmatched case?
    Nothing ->
      pure
        (Ok
           (CaseExpression
              (Case {scrutinee = scrutinee', location = SteppedCursor, ..})))

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

stepGlobal :: Global Resolved -> Step (Result (Expression Resolved))
stepGlobal global@Global {name} = do
  hashes <- ask
  case name of
    HashGlobal hash ->
      case M.lookup hash hashes of
        Just expre -> pure (pure expre)
        Nothing -> pure (Errored (NotInScope hash))
    _ -> pure (pure (GlobalExpression global))

--------------------------------------------------------------------------------
-- Function application

stepApply :: Apply Resolved -> Step (Result (Expression Resolved))
stepApply Apply {..} = do
  function' <- stepExpression function?
  argument' <- stepExpression argument?
  let apply' = Apply {function = function', argument = argument', ..}
  -- tracePrinter (apply') (pure ())
  case apply' of
    Apply { argument = ArrayExpression list
          , typ = listApplyType
          , function = ApplyExpression Apply { argument = fromIntegerOp
                                             , typ = fromIntegerApplyType
                                             , function = ApplyExpression Apply { argument = divideOp
                                                                                , typ = divideOpTyp
                                                                                , function = ApplyExpression Apply { argument = addOp
                                                                                                                   , typ = addOpType
                                                                                                                   , function = GlobalExpression Global {name = FunctionGlobal AverageFunction}
                                                                                                                   }
                                                                                }
                                             }
          } ->
      stepAverage
        (list, listApplyType)
        (addOp, addOpType)
        (divideOp, divideOpTyp)
        (fromIntegerOp, fromIntegerApplyType)
    Apply { argument = ArrayExpression list
          , typ = listApplyType
          , function = ApplyExpression Apply { argument = fromIntegerOp
                                             , typ = fromIntegerApplyType
                                             , function = GlobalExpression Global {name = FunctionGlobal LengthFunction}
                                             }
          } ->
      stepLength (list, listApplyType) (fromIntegerOp, fromIntegerApplyType)
    Apply { argument = ArrayExpression list
          , typ = listApplyType
          , function = ApplyExpression Apply { argument = compareOp
                                             , function = GlobalExpression Global {name = FunctionGlobal SortFunction}
                                             }
          } -> stepSort (ApplyExpression apply') (list, listApplyType) compareOp
    Apply { argument = ArrayExpression list
          , typ = listApplyType
          , function = ApplyExpression Apply { argument = predicate
                                             , function = GlobalExpression Global {name = FunctionGlobal FindFunction}
                                             }
          } -> stepFind (ApplyExpression apply') (list, listApplyType) predicate
    Apply { argument = ArrayExpression list
          , typ = listApplyType
          , function = ApplyExpression Apply { argument = predicate
                                             , function = GlobalExpression Global {name = FunctionGlobal AllFunction}
                                             }
          } -> stepAll (ApplyExpression apply') (list, listApplyType) predicate
    Apply { argument = ArrayExpression list
          , typ = listApplyType
          , function = ApplyExpression Apply { argument = predicate
                                             , function = GlobalExpression Global {name = FunctionGlobal AnyFunction}
                                             }
          } -> stepAny (ApplyExpression apply') (list, listApplyType) predicate
    Apply { argument = ArrayExpression list
          , typ = listApplyType
          , function = ApplyExpression Apply { argument = compareOp
                                             , function = GlobalExpression Global {name = FunctionGlobal DistinctFunction}
                                             }
          } ->
      stepDistinct (ApplyExpression apply') (list, listApplyType) compareOp
    Apply { argument = ArrayExpression list
          , typ = listApplyType
          , function = ApplyExpression Apply { argument = compareOp
                                             , function = GlobalExpression Global {name = FunctionGlobal MaximumFunction}
                                             }
          } ->
      stepMaximum (ApplyExpression apply') (list, listApplyType) compareOp
    Apply { argument = ArrayExpression list
          , typ = listApplyType
          , function = ApplyExpression Apply { argument = compareOp
                                             , function = GlobalExpression Global {name = FunctionGlobal MinimumFunction}
                                             }
          } ->
      stepMinimum (ApplyExpression apply') (list, listApplyType) compareOp
    Apply {argument=ArrayExpression list, typ = returnedType, function = GlobalExpression Global {name=FunctionGlobal ConcatFunction}} ->
      stepConcat (ApplyExpression apply') list returnedType
    Apply { argument = ArrayExpression list
          , typ = listApplyType
          , function = ApplyExpression Apply { argument = fromIntegerOp
                                             , typ = fromIntegerApplyType
                                             , function = ApplyExpression Apply { argument = addOp
                                                                                , typ = addOpType
                                                                                , function = GlobalExpression Global {name = FunctionGlobal SumFunction}
                                                                                }
                                             }
          } ->
      stepSum
        (list, listApplyType)
        (addOp, addOpType)
        (fromIntegerOp, fromIntegerApplyType)
    _ -> do
      case function' of
        LambdaExpression Lambda {body} -> do
          body' <- betaReduce body argument'
          stepExpression body'
        GlobalExpression (Global {name = FunctionGlobal func})
          | elem func [NullFunction] -> stepFunction1 typ func argument'
        ApplyExpression Apply { function = GlobalExpression (Global {name = FromIntegerGlobal})
                              , argument = GlobalExpression (Global {name = (InstanceGlobal FromIntegerIntegerInstance)})
                              }
          | LiteralExpression (NumberLiteral (Number {number = IntegerNumber {}})) <-
             argument' -> pure (Ok argument')
        ApplyExpression Apply { function = GlobalExpression (Global {name = FromDecimalGlobal})
                              , argument = GlobalExpression (Global {name = (InstanceGlobal (FromDecimalDecimalInstance fromDecimalInstance))})
                              }
          | LiteralExpression (NumberLiteral (Number { number = DecimalNumber decimal
                                                     , typ = typ'
                                                     })) <- argument' -> do
            decimal' <- (pure (fromDecimalStep fromDecimalInstance decimal))?
            pure
              (Ok
                 (LiteralExpression
                    (NumberLiteral
                       (Number {number = DecimalNumber decimal', typ = typ', ..}))))
        ApplyExpression Apply { function = GlobalExpression (Global {name = FromIntegerGlobal})
                              , argument = GlobalExpression (Global {name = (InstanceGlobal (FromIntegerDecimalInstance supersetPlaces))})
                              }
          | LiteralExpression (NumberLiteral (Number { number = IntegerNumber integer
                                                     , typ = typ'
                                                     })) <- argument' -> do
            pure
              (Ok
                 (LiteralExpression
                    (NumberLiteral
                       (Number
                          { number =
                              DecimalNumber
                                (decimalFromInteger integer supersetPlaces)
                          , typ = typ'
                          , ..
                          }))))
        ApplyExpression Apply { function = GlobalExpression Global {name = FunctionGlobal functionName}
                              , argument = functionExpression
                              , location = applyLocation
                              }
          | functionName `elem` [MapFunction, FilterFunction, FromOkFunction] ->
            stepFunction2
              functionName
              argument'
              functionExpression
              location
              applyLocation
              typ
        _ ->
          pure
            (Ok
               (ApplyExpression
                  Apply {function = function', argument = argument', ..}))

--------------------------------------------------------------------------------
-- Function stepper

stepConcat ::
     Expression Resolved
  -> Array Resolved
  -> Type Generalised
  -> Step (Result (Expression Resolved))
stepConcat easis (array@Array {expressions}) returnedType = do
  result <-
    traverseE
      (\e -> do
         e' <- stepExpression e?
         case e' of
           HoleExpression {} -> pure (FoundHole e')
           ArrayExpression Array {expressions = es} -> pure (Ok es)
           _ -> pure (Errored NeededArrayForConcat))
      expressions
  case result of
    Ok xs ->
      pure
        (Ok
           (ArrayExpression
              (array
                 { Array.typ = returnedType
                 , expressions = V.concat (V.toList xs)
                 , Array.location = SteppedCursor
                 , form = Evaluated
                 })))
    Returned e -> pure (Ok (variantSigged okTagName returnedType (pure e)))
    FoundHole {} -> pure (Ok easis)
    Errored e -> pure (Errored e)

stepFind ::
     Expression Resolved
  -> (Array Resolved, Type Generalised)
  -> Expression Resolved
  -> Step (Result (Expression Resolved))
stepFind asis (Array {expressions}, typ) predicate =
  if null expressions
    then pure (Ok (variantSigged (TagName "find_empty") typ Nothing))
    else do
      result <-
        traverseE_
          (\e -> do
             e' <- stepExpression e?
             case e' of
               HoleExpression {} -> pure (FoundHole e')
               _ -> do
                 boolish <-
                   stepExpression
                     (ApplyExpression
                        (Apply
                           { function = predicate
                           , argument = e'
                           , location = BuiltIn
                           , typ = typeOutput (expressionType predicate)
                           }))?
                 boolR <- reify boolish
                 case boolR of
                   Ok (BoolR _ True) -> pure (Returned e')
                   Ok (BoolR _ False) -> pure (Ok ())
                   Ok _ -> pure (Errored ShouldGetBool)
                   Returned r' -> pure (Errored (EarlyReturnWithoutBoundary r'))
                   FoundHole h -> pure (FoundHole h)
                   Errored err -> pure (Errored err))
          (toList expressions)
      case result of
        Returned e -> pure (Ok (variantSigged okTagName typ (pure e)))
        FoundHole {} -> pure (Ok asis)
        Errored e -> pure (Errored e)
        Ok () -> pure (Ok (variantSigged (TagName "find_failed") typ Nothing))

-- | Any is a bool-specific version of find.
stepAny ::
     Expression Resolved
  -> (Array Resolved, Type Generalised)
  -> Expression Resolved
  -> Step (Result (Expression Resolved))
stepAny asis (Array {expressions}, typ) predicate = do
  if V.null expressions
    then pure (Ok (variantSigged (TagName "any_empty") typ Nothing))
    else do
      result <-
        traverseE_
          (\e -> do
             e' <- stepExpression e?
             case e' of
               HoleExpression {} -> pure (FoundHole e')
               _ -> do
                 boolish <-
                   stepExpression
                     (ApplyExpression
                        (Apply
                           { function = predicate
                           , argument = e'
                           , location = BuiltIn
                           , typ = typeOutput (expressionType predicate)
                           }))?
                 boolR <- reify boolish
                 case boolR of
                   Ok (BoolR _ True) ->
                     pure (Returned (trueVariant BuiltIn))
                   Ok (BoolR _ False) -> pure (Ok ())
                   Ok _ -> pure (Errored ShouldGetBool)
                   Returned r' -> pure (Errored (EarlyReturnWithoutBoundary r'))
                   FoundHole h -> pure (FoundHole h)
                   Errored err -> pure (Errored err))
          (toList expressions)
      case result of
        Ok () -> pure (Ok (variantSigged okTagName typ (pure (falseVariant BuiltIn))))
        Returned e -> pure (Ok (variantSigged okTagName typ (pure e)))
        FoundHole{} -> pure (Ok asis)
        Errored e -> pure (Errored e)

-- | All is a bool-specific version of find.
stepAll ::
     Expression Resolved
  -> (Array Resolved, Type Generalised)
  -> Expression Resolved
  -> Step (Result (Expression Resolved))
stepAll asis (Array {expressions}, typ) predicate = do
  if V.null expressions
    then pure (Ok (variantSigged (TagName "all_empty") typ Nothing))
    else do
      result <-
        traverseE_
          (\e -> do
             e' <- stepExpression e?
             case e' of
               HoleExpression {} -> pure (FoundHole e')
               _ -> do
                 boolish <-
                   stepExpression
                     (ApplyExpression
                        (Apply
                           { function = predicate
                           , argument = e'
                           , location = BuiltIn
                           , typ = typeOutput (expressionType predicate)
                           }))?
                 boolR <- reify boolish
                 case boolR of
                   Ok (BoolR _ True) -> pure (Ok ())
                   Ok (BoolR _ False) -> pure (Returned (falseVariant BuiltIn))
                   Ok _ -> pure (Errored ShouldGetBool)
                   Returned r' -> pure (Errored (EarlyReturnWithoutBoundary r'))
                   FoundHole h -> pure (FoundHole h)
                   Errored err -> pure (Errored err))
          (toList expressions)
      case result of
        Returned false -> pure (Ok (variantSigged okTagName typ (pure false)))
        Ok () -> pure (Ok (variantSigged okTagName typ (pure (trueVariant BuiltIn))))
        FoundHole{} -> pure (Ok asis)
        Errored e -> pure (Errored e)

stepDistinct ::
     Expression Resolved
  -> (Array Resolved, Type Generalised)
  -> Expression Resolved
  -> Step (Result (Expression Resolved))
stepDistinct asis (list@Array {expressions}, _listApplyType) _cmpInst = do
  result <-
    traverseE
      (\e -> do
         e' <- stepExpression e?
         reify e')
      (toList expressions)
  case result of
    FoundHole {} -> pure (Ok asis)
    Ok es ->
      pure
        (Ok
           (ArrayExpression
              list
                { expressions = V.fromList (map originalR (nubOrd es))
                , Array.location = SteppedCursor
                , form = Evaluated
                }))
    Errored e -> pure (Errored e)
    Returned e -> pure (Returned e)

stepSort ::
     Expression Resolved
  -> (Array Resolved, Type Generalised)
  -> Expression Resolved
  -> Step (Result (Expression Resolved))
stepSort asis (list@Array {expressions}, _listApplyType) _cmpInst = do
  result <-
    traverseE
      (\e -> do
         e' <- stepExpression e?
         reify e')
      (toList expressions)
  case result of
    FoundHole {} -> pure (Ok asis)
    Returned r' -> pure (Errored (EarlyReturnWithoutBoundary r'))
    Errored err -> pure (Errored err)
    Ok es ->
      pure
        (Ok
           (ArrayExpression
              list
                { expressions = V.fromList (map originalR (List.sort es))
                , Array.location = SteppedCursor
                , form = Evaluated
                }))

stepMinimum ::
     Expression Resolved
  -> (Array Resolved, Type Generalised)
  -> Expression Resolved
  -> Step (Result (Expression Resolved))
stepMinimum asis (Array {expressions}, typ) _cmpInst = do
  result <-
    traverseE
      (\e -> do
         e' <- stepExpression e?
         reify e')
      (toList expressions)
  case result of
    FoundHole {} -> pure (Ok asis)
    Returned r' -> pure (Errored (EarlyReturnWithoutBoundary r'))
    Errored err -> pure (Errored err)
    Ok [] -> pure (Ok (variantSigged (TagName "minimum_empty") typ Nothing))
    Ok es -> pure (Ok (variantSigged okTagName typ (pure (originalR (minimum es)))))

stepMaximum ::
     Expression Resolved
  -> (Array Resolved, Type Generalised)
  -> Expression Resolved
  -> Step (Result (Expression Resolved))
stepMaximum asis (Array {expressions}, typ) _cmpInst = do
  result <-
    traverseE
      (\e -> do
         e' <- stepExpression e?
         reify e')
      (toList expressions)
  case result of
    FoundHole {} -> pure (Ok asis)
    Returned r' -> pure (Errored (EarlyReturnWithoutBoundary r'))
    Errored err -> pure (Errored err)
    Ok [] -> pure (Ok (variantSigged empty_case typ Nothing))
    Ok es ->
      pure
        (Ok (variantSigged okTagName typ (pure (originalR (maximum es)))))
  where
    empty_case = TagName "maximum_empty"

stepAverage ::
     (Array Resolved, Type Generalised)
  -> (Expression Resolved, Type Generalised)
  -> (Expression Resolved, Type Generalised)
  -> (Expression Resolved, Type Generalised)
  -> Step (Result (Expression Resolved))
stepAverage list@(Array {expressions}, listApplyType) (addOp, _) (divideOp, _) fromInt =
  loop Nothing (V.toList expressions)
  where
    loop nil [] =
      case nil of
        Nothing -> pure (Ok (variantSigged (TagName "average_empty") listApplyType Nothing))
        Just total -> do
          lenE <- stepLength list fromInt?
          avgE <-
            stepInfix
              Infix
                { location = BuiltIn
                , global =
                    ApplyExpression
                      Apply
                        { location = BuiltIn
                        , typ = listApplyType -- The types here matter.
                        , function =
                            GlobalExpression
                              Global
                                { location = BuiltIn
                                , scheme = ResolvedScheme (binOpType DivideOp)
                                , name = NumericBinOpGlobal DivideOp
                                }
                        , argument = divideOp
                        }
                , left = total
                , right = lenE
                , typ = listApplyType
                }?
          pure (Ok (variantSigged okTagName listApplyType (pure avgE)))
    loop nil (e:es) =
      case nil of
        Nothing -> loop (Just e) es
        Just acc -> do
          nil' <-
            stepInfix
              Infix
                { location = BuiltIn
                , global =
                    ApplyExpression
                      Apply
                        { location = BuiltIn
                        , typ = listApplyType -- The types here matter.
                        , function =
                            GlobalExpression
                              Global
                                { location = BuiltIn
                                , scheme = ResolvedScheme (binOpType AddOp)
                                , name = NumericBinOpGlobal AddOp
                                }
                        , argument = addOp
                        }
                , left = acc
                , right = e
                , typ = listApplyType
                }?
          loop (Just nil') es

stepLength ::
     (Array Resolved, Type Generalised)
  -> (Expression Resolved, Type Generalised)
  -> Step (Result (Expression Resolved))
stepLength (Array {expressions}, listApplyType) (fromIntegerOp, _fromIntegerType) =
  stepApply
    Apply
      { location = BuiltIn
      , function =
          ApplyExpression
            Apply
              { function =
                  GlobalExpression
                    (Global
                       { location = BuiltIn
                       , scheme = ResolvedScheme (nullType BuiltIn) -- TODO
                       , name = FromIntegerGlobal
                       })
              , typ = listApplyType
              , argument = fromIntegerOp
              , location = BuiltIn
              }
      , typ = listApplyType
      , argument =
          LiteralExpression
            (NumberLiteral
               Number
                 { number = IntegerNumber (fromIntegral (V.length expressions))
                 , location = SteppedCursor
                 , typ = listApplyType
                 })
      }

stepSum ::
     (Array Resolved, Type Generalised)
  -> (Expression Resolved, Type Generalised)
  -> (Expression Resolved, Type Generalised)
  -> Step (Result (Expression Resolved))
stepSum (Array {expressions}, listApplyType) (addOp, _addOpType) (_fromIntegerOp, _fromIntegerType) =
  loop Nothing (V.toList expressions)
  where
    loop nil [] =
      case nil of
        Nothing -> pure (Ok (variantSigged (TagName "sum_empty") listApplyType Nothing))
        Just thing -> pure (Ok (variantSigged okTagName listApplyType (pure thing)))
    loop nil (e:es) =
      case nil of
        Nothing -> loop (Just e) es
        Just acc -> do
          nil' <-
            stepInfix
              Infix
                { location = BuiltIn
                , global =
                    ApplyExpression
                      Apply
                        { location = BuiltIn
                        , typ = listApplyType -- The types here matter.
                        , function =
                            GlobalExpression
                              Global
                                { location = BuiltIn
                                , scheme = ResolvedScheme (binOpType AddOp)
                                , name = NumericBinOpGlobal AddOp
                                }
                        , argument = addOp
                        }
                , left = acc
                , right = e
                , typ = listApplyType
                }?
          loop (Just nil') es

stepFunction1 ::
     Type Generalised
  -> Function
  -> Expression Resolved
  -> Step (Result (Expression Resolved))
stepFunction1 returnType func argument =
  case func of
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
  -> Step (Result (Expression Resolved))
stepFunction2 function argument' functionExpression location _applyLocation originalArrayType =
  case function of
    FromOkFunction ->
      do result <- stepApply
           Apply
             { location = BuiltIn
             , function = ApplyExpression Apply
                                            { location = BuiltIn
                                            , function = from_okFunction
                                            , argument = functionExpression
                                            , typ = typeOutput (expressionType functionExpression)
                                            }
             , argument = argument'
             , typ = originalArrayType
             }
         pure result
    MapFunction ->
      case argument' of
        ArrayExpression Array {expressions} -> do
          expressions' <-
            traverseE
              (\arrayItem ->
                 stepExpression
                   (ApplyExpression
                      (Apply
                         { function = functionExpression
                         , argument = arrayItem
                         , location = location
                         , typ = typeOutput (expressionType functionExpression)
                         })))
              expressions?
          pure
            (Ok
               (ArrayExpression
                  Array
                    { typ =
                        ArrayType
                          (typeOutput (expressionType functionExpression))
                    , location = SteppedCursor
                    , expressions = expressions'
                    , form = Evaluated
                    }))
        _ -> error "Invalid argument to function."
    FilterFunction ->
      case argument' of
        ArrayExpression Array {expressions, typ} -> do
          expressions' <-
            traverseE
              (\arrayItem -> do
                 arrayItem' <- stepExpression arrayItem?
                 bool <-
                   stepExpression
                     (ApplyExpression
                        (Apply
                           { function = functionExpression
                           , argument = arrayItem'
                           , location = location
                           , typ =
                               typeOutput (expressionType functionExpression)
                           }))?
                 boolR <- reify bool
                 case boolR of
                   Ok (BoolR _ True) -> pure (Ok (Just arrayItem'))
                   Ok (BoolR _ False) -> pure (Ok Nothing)
                   Ok _ -> pure (Errored ShouldGetBool)
                   Returned r' -> pure (Errored (EarlyReturnWithoutBoundary r'))
                   FoundHole e -> pure (Ok (Just e))
                   Errored e -> pure (Errored e))
              expressions?
          pure
            (Ok
               (ArrayExpression
                  Array
                    { typ
                    , location = SteppedCursor
                    , expressions = (V.mapMaybe id expressions')
                    , form = Evaluated
                    }))
        _ -> error "Invalid argument to function."
    _ -> error "TODO: Missing function implementation!"

--------------------------------------------------------------------------------
-- Infix stepper

stepInfix :: Infix Resolved -> Step (Result (Expression Resolved))
stepInfix Infix {..} = do
  global' <- stepExpression global?
  left' <- stepExpression left?
  right' <- stepExpression right?
  let asis =
        (InfixExpression
           Infix
             { global = global'
             , left = left'
             , right = right'
             , location = SteppedCursor
             , ..
             })
  case (left', right') of
    (HoleExpression {}, _) -> pure (Ok asis)
    (_, HoleExpression {}) -> pure (Ok asis)
    _ ->
      case global'
        -- Arithmetic
            of
        ApplyExpression Apply { function = GlobalExpression Global {name = NumericBinOpGlobal {}}
                              , argument = GlobalExpression Global {name = InstanceGlobal (IntegerOpInstance numericBinOp)}
                              } -> stepIntegerOp asis numericBinOp left' right'
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
  -> f (Result (Expression Resolved))
stepAtomicEquality asis location equality left' right' =
  case (left', right') of
    (LiteralExpression (NumberLiteral Number {number = left}), LiteralExpression (NumberLiteral Number {number = right})) -> do
      pure
        (Ok (if comparator left right
            then trueVariant location
            else falseVariant location))
    (LiteralExpression (TextLiteral LiteralText {text = left}), LiteralExpression (TextLiteral LiteralText {text = right})) -> do
      pure
        (Ok (if comparator left right
            then trueVariant location
            else falseVariant location))
    _ -> pure (Ok asis)
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
  -> f (Result (Expression Resolved))
stepAtomicComparison asis location compareity left' right' = fmap Ok $
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
  -> Step (Result (Expression Resolved))
stepIntegerOp asis numericBinOp left' right' =
  case (left', right') of
    (LiteralExpression (NumberLiteral Number {number = IntegerNumber left, typ}), LiteralExpression (NumberLiteral Number {number = IntegerNumber right}))
      | DivideOp <- numericBinOp
      , 0 <- right -> pure (Ok asis) -- Nothing to do for division by zero.
      | otherwise -> do
        pure
          (Ok
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
                     })))
    _ {-Step (lift (lift (Left (InvalidIntegerOpOperands left' right'))))-}
     -- warn
     -> pure (Ok asis)

stepDecimalOp ::
     Expression Resolved
  -> Natural
  -> NumericBinOp
  -> Expression Resolved
  -> Expression Resolved
  -> Step (Result (Expression Resolved))
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
        Nothing -> pure (Errored MismatchingPrecisionsInOp)
        Just (Left ()) -> pure (Ok asis) -- Division by zero has no answer, so we stop.
        Just (Right result) ->
          pure
            (Ok (LiteralExpression
                (NumberLiteral
                   Number
                     { number = DecimalNumber result
                     , location = SteppedCursor
                     -- TODO: Add "step number X" or something to say
                     -- "this is where the value came from". Could be
                     -- useful for finding 0's which hit an x/0.
                     , ..
                     })))
    -- _ -> Step (lift (lift (Left (InvalidDecimalOpOperands left' right'))))
    _ -> pure (Ok asis)

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
-- FromDecimal instance stepping

fromDecimalStep :: FromDecimalInstance -> Decimal -> Result Decimal
fromDecimalStep fromDecimalInstance decimal =
  if thisSubsetPlaces == subsetPlaces
    then if thisSubsetPlaces == supersetPlaces
           then Ok decimal
           else if thisSubsetPlaces < supersetPlaces
                  then Ok (expandDecimalPrecision supersetPlaces decimal)
                  else Errored
                         (CannotShrinkADecimalFromTo
                            thisSubsetPlaces
                            supersetPlaces)
    else Errored
           (MismatchingPrecisionsInFromDecimal thisSubsetPlaces subsetPlaces)
  where
    Decimal {places = thisSubsetPlaces} = decimal
    FromDecimalInstance {supersetPlaces, subsetPlaces} = fromDecimalInstance

--------------------------------------------------------------------------------
-- Helpers

holeExpression :: (StagedLocation s ~ Cursor) => StagedType s -> Expression s
holeExpression typ = HoleExpression Hole {location = BuiltIn, typ}

--------------------------------------------------------------------------------
-- Reification

-- Define a specific reification result type like Reified | GotHole
-- rewrite the one in function stepper

data Reified e
  = TextR e !Text
  | IntegerR e !Integer
  | DecimalR e !Decimal
  | BoolR e !Bool
  deriving (Functor, Traversable, Foldable)

instance Eq (Reified e) where
  (==) x y = case (x,y) of
               (TextR _ a, TextR _ b) -> a == b
               (IntegerR _ a, IntegerR _ b) -> a == b
               (DecimalR _ a, DecimalR _ b) -> a == b
               (BoolR _ a, BoolR _ b) -> a == b
               _ -> error "Invalid Eq comparison between types."

instance Ord (Reified e) where
  compare x y = case (x,y) of
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

reify :: Expression Resolved -> Step (Result (Reified (Expression Resolved)))
reify e' =
  case e' of
    LiteralExpression literal ->
      case literal of
        TextLiteral LiteralText {text} -> pure (Ok (TextR e' text))
        NumberLiteral Number {number = IntegerNumber integer} ->
          pure (Ok (IntegerR e' integer))
        NumberLiteral Number {number = DecimalNumber decimal} ->
          pure (Ok (DecimalR e' decimal))
    VariantExpression Variant {tag = TagName "true", argument = Nothing} ->
      pure (Ok (BoolR e' True))
    VariantExpression Variant {tag = TagName "false", argument = Nothing} ->
      pure (Ok (BoolR e' False))
    HoleExpression {} -> pure (FoundHole e')
    _ -> pure (FoundHole e') -- This could error out, but in the case
                             -- of _(1) for example, the hole is there
                             -- but nested. So for now we leave it as a hole find.
