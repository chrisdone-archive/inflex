{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Test the solver with GHC as reference implementation.

module SolverPropSpec where

import           Data.Foldable
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Inflex.Types
import           Language.Haskell.Interpreter
import           Test.Hspec

--------------------------------------------------------------------------------
-- Test suite

spec :: Spec
spec =
  it
    "Hint"
    (do pendingWith "Implemented example, but slow; needs property testing."
        shouldReturn
          (runInterpreter
             (do set [languageExtensions := [GADTs]]
                 setImportsQ [("Prelude", Nothing)]
                 typeOf
                   "undefined :: (t ~ (->) a a, (->) a a ~ (->) (Maybe b) (Maybe Integer)) => t"))
          (Right "Maybe Integer -> Maybe Integer"))

--------------------------------------------------------------------------------
-- Rendering to Haskell string

typeHsString :: Type Generated -> String
typeHsString = toList . typeHs

typeHs :: Type Generated -> Seq Char
typeHs =
  \case
    VariableType typeVariable -> typeVariableHs typeVariable
    ConstantType typeConstant -> typeConstantHs typeConstant
    ApplyType TypeApplication {function, argument} ->
      case function of
        ApplyType TypeApplication {function = ConstantType TypeConstant {name = FunctionTypeName}} ->
          typeHs function <> Seq.fromList " -> " <> typeHs argument
        _ -> typeHs function <> Seq.fromList " " <> paren (typeHs argument)
      where paren x =
              if any (== ' ') (toList x)
                then Seq.fromList "(" <> x <> Seq.fromList ")"
                else x

typeVariableHs :: TypeVariable Generated -> Seq Char
typeVariableHs TypeVariable {index} = Seq.fromList (show index)

typeConstantHs :: TypeConstant Generated -> Seq Char
typeConstantHs TypeConstant {name=n} =
  Seq.fromList
    (case n of
       FunctionTypeName -> "(->)"
       IntegerTypeName -> "Integer"
       TextTypeName -> "Text"
       OptionTypeName -> "Maybe")

--------------------------------------------------------------------------------
-- Orphans

deriving instance Eq InterpreterError
deriving instance Eq GhcError
