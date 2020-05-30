{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- |

module SolverSpec where

import           Inflex.Instances ()
import           Inflex.Solver
import           Inflex.Types
import           Test.Hspec

spec :: Spec
spec = do
  describe
    "Successful"
    (do it "a ~ a" (shouldBe (unifyConstraints [a .~ a]) (pure []))
        it
          "Integer ~ Integer"
          (shouldBe (unifyConstraints [_Integer .~ _Integer]) (pure []))
        it
          "a ~ b"
          (do pending
              shouldBe (unifyConstraints [a .~ b]) (pure [a' .+-> b]))
        it
          "a ~ Integer"
          (do pending
              shouldBe
                (unifyConstraints [a .~ _Integer])
                (pure [a' .+-> _Integer]))
        it
          "F a Text ~ F Integer b"
          (do pending
              shouldBe
                (unifyConstraints [_F a _Text .~ _F _Integer b])
                (pure [a' .+-> _Integer, b' .+-> _Text]))
        it
          "F a a ~ F (Option b) (Option Integer)"
          (do pending
              shouldBe
                (unifyConstraints [_F a a .~ _F (_Option b) (_Option _Integer)])
                (pure [a' .+-> _Option b, b' .+-> _Integer])))
  describe
    "Failing"
    (do it
          "Integer ~ Text"
          (do pending
              shouldBe
                (unifyConstraints [_Integer .~ _Text])
                (Left (pure ConstantMismatch)))
        it
          "F a a ~ F (Option Text) (Option Integer)"
          (do pending
              shouldBe
                (unifyConstraints
                   [_F a a .~ _F (_Option _Text) (_Option _Integer)])
                (Left (pure ConstantMismatch))))

--------------------------------------------------------------------------------
-- Type variables

a' :: TypeVariable Generated
a' =
  TypeVariable
    {location = ExpressionCursor, prefix = IntegeryPrefix, index = 0}

b' :: TypeVariable Generated
b' =
  TypeVariable
    {location = ExpressionCursor, prefix = IntegeryPrefix, index = 1}

c' :: TypeVariable Generated
c' =
  TypeVariable
    {location = ExpressionCursor, prefix = IntegeryPrefix, index = 2}

--------------------------------------------------------------------------------
-- Types of the variables

a :: Type Generated
a = VariableType a'

b :: Type Generated
b = VariableType b'

c :: Type Generated
c = VariableType c'

--------------------------------------------------------------------------------
-- Type constructors

_Integer :: Type Generated
_Integer =
  ConstantType
    TypeConstant {location = ExpressionCursor, name = IntegerTypeName}

_Text :: Type Generated
_Text =
  ConstantType
    TypeConstant {location = ExpressionCursor, name = TextTypeName}

_F :: Type Generated -> Type Generated -> Type Generated
_F x1 x2 =
  ApplyType
    TypeApplication
      { location = ExpressionCursor
      , function =
          ApplyType
            TypeApplication
              { location = ExpressionCursor
              , function =
                  ConstantType
                    TypeConstant
                      {location = ExpressionCursor, name = FunctionTypeName}
              , argument = x1
              }
      , argument = x2
      }

_Option :: Type Generated -> Type Generated
_Option x1 =
  ApplyType
    TypeApplication
      { location = ExpressionCursor
      , function =
          ConstantType
            TypeConstant
              {location = ExpressionCursor, name = OptionTypeName}
      , argument = x1
      }

--------------------------------------------------------------------------------
-- Operators for easier reading

(.~) :: Type Generated -> Type Generated -> EqualityConstraint
(.~) x y =
  EqualityConstraint {location = ExpressionCursor, type1 = x, type2 = y}

(.+->) :: TypeVariable Generated -> Type Generated -> Substitution
(.+->) x y = Substitution {before = x, after = y}
