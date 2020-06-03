{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Test the solver with GHC as reference implementation.

module SolverPropSpec where

import           Control.Monad
import           Data.Bifunctor
import           Data.Foldable
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Inflex.Instances ()
import           Inflex.Solver
import           Inflex.Types
import           Instances ()
import           Language.Haskell.Interpreter
import           Test.Hspec
import           Test.QuickCheck
import           Test.Validity

--------------------------------------------------------------------------------
-- Test suite

spec :: Spec
spec =
  when
    False
    (it
       "Hint"
       (property
          (forAllValid
             (\eq@(EqualityConstraint {type1}) -> do
                let input =
                      "undefined :: (t ~ (" <> typeHsString type1 <> "), " <>
                      equalityString eq <>
                      ") => t"
                print input
                shouldReturn
                  (fmap
                     (first (const ()))
                     (runInterpreter
                        (do set [languageExtensions := [GADTs]]
                            setImportsQ [("Prelude", Nothing)]
                            fmap clean (typeOf input))))
                  (fmap
                     (\cs -> clean (typeHsString (solveType cs type1)))
                     (first (const ()) (unifyEqualityConstraint eq)))))))
  where
    clean = unwords . words

--------------------------------------------------------------------------------
-- Rendering to Haskell string

equalityString :: EqualityConstraint -> String
equalityString EqualityConstraint {type1, type2} =
  "(" <> typeHsString type1 <> ") ~ (" <> typeHsString type2 <> ")"

typeHsString :: Type s -> String
typeHsString = toList . typeHs

typeHs :: Type s -> Seq Char
typeHs =
  \case
    VariableType typeVariable -> typeVariableHs typeVariable
    ConstantType typeConstant -> typeConstantHs typeConstant
    ApplyType TypeApplication {function = function0, argument} ->
      case function0 of
        ApplyType TypeApplication { function = ConstantType TypeConstant {name = FunctionTypeName}
                                  , argument = function'
                                  } ->
          typeHs function' <> Seq.fromList " -> " <> typeHs argument
        _ -> typeHs function0 <> Seq.fromList " " <> paren (typeHs argument)
      where paren x =
              if any (== ' ') (toList x)
                then Seq.fromList "(" <> x <> Seq.fromList ")"
                else x

typeVariableHs :: TypeVariable s -> Seq Char
typeVariableHs TypeVariable {index} = Seq.fromList ("t" <> show index)

typeConstantHs :: TypeConstant s -> Seq Char
typeConstantHs TypeConstant {name=n} =
  Seq.fromList
    (case n of
       FunctionTypeName -> "(->)"
       IntegerTypeName -> "Integer"
       TextTypeName -> "[Char]"
       OptionTypeName -> "Maybe")

--------------------------------------------------------------------------------
-- Orphans

deriving instance Eq InterpreterError
deriving instance Eq GhcError
