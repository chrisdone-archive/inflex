{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- |

module SolverSpec where

import qualified Data.Map.Strict as M
import           Inflex.Instances ()
import           Inflex.Solver
import           Inflex.Types
import           Test.Hspec

spec :: Spec
spec = do describe "Fine-grained" fineGrained
          describe "Coarse-grained" coarseGrained

--------------------------------------------------------------------------------
-- Coarse-grained tests

coarseGrained :: Spec
coarseGrained =
  describe
    "Successful"
    (do it
          "123"
          (shouldBe
             (solveText "" "123")
             (Right (IsSolved {thing = LiteralExpression (NumberLiteral (Number {location = ExpressionCursor, number = IntegerNumber 123, typ = ConstantType (TypeConstant {location = ExpressionCursor, name = IntegerTypeName})})), mappings = M.fromList [(ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 1, name = ""}, end = SourcePos {line = 1, column = 4, name = ""}})]})))
        it
          "(\\x->x)123"
          (shouldBe
             (solveText "" "(\\x->x)123")
             (Right (IsSolved {thing = ApplyExpression (Apply {location = ExpressionCursor, function = LambdaExpression (Lambda {location = ApplyFuncCursor ExpressionCursor, param = Param {location = ApplyFuncCursor LambdaParamCursor, name = (), typ = ConstantType (TypeConstant {location = ApplyArgCursor ExpressionCursor, name = IntegerTypeName})}, body = VariableExpression (Variable {location = ApplyFuncCursor (LambdaBodyCursor ExpressionCursor), name = DeBrujinIndex 0, typ = ConstantType (TypeConstant {location = ApplyArgCursor ExpressionCursor, name = IntegerTypeName})}), typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = ApplyFuncCursor ExpressionCursor, name = FunctionTypeName}), argument = ConstantType (TypeConstant {location = ApplyArgCursor ExpressionCursor, name = IntegerTypeName}), location = ApplyFuncCursor ExpressionCursor, kind = FunKind TypeKind TypeKind}), argument = ConstantType (TypeConstant {location = ApplyArgCursor ExpressionCursor, name = IntegerTypeName}), location = ApplyFuncCursor ExpressionCursor, kind = TypeKind})}), argument = LiteralExpression (NumberLiteral (Number {location = ApplyArgCursor ExpressionCursor, number = IntegerNumber 123, typ = ConstantType (TypeConstant {location = ApplyArgCursor ExpressionCursor, name = IntegerTypeName})})), typ = ConstantType (TypeConstant {location = ApplyArgCursor ExpressionCursor, name = IntegerTypeName})}), mappings = M.fromList [(ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 2, name = ""}, end = SourcePos {line = 1, column = 7, name = ""}}),(ApplyFuncCursor ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 2, name = ""}, end = SourcePos {line = 1, column = 7, name = ""}}),(ApplyFuncCursor (LambdaBodyCursor ExpressionCursor),SourceLocation {start = SourcePos {line = 1, column = 6, name = ""}, end = SourcePos {line = 1, column = 7, name = ""}}),(ApplyFuncCursor LambdaParamCursor,SourceLocation {start = SourcePos {line = 1, column = 3, name = ""}, end = SourcePos {line = 1, column = 4, name = ""}}),(ApplyArgCursor ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 8, name = ""}, end = SourcePos {line = 1, column = 11, name = ""}})]}))))


--------------------------------------------------------------------------------
-- Fine-grained tests

fineGrained :: Spec
fineGrained = do
  describe
    "Successful"
    (do it "a ~ a" (shouldBe (unifyConstraints [a .~ a]) (pure []))
        it
          "Integer ~ Integer"
          (shouldBe (unifyConstraints [_Integer .~ _Integer]) (pure []))
        it "a ~ b" (shouldBe (unifyConstraints [a .~ b]) (pure [a' .+-> b]))
        it
          "a ~ Integer"
          (shouldBe (unifyConstraints [a .~ _Integer]) (pure [a' .+-> _Integer]))
        it
          "F a Text ~ F Integer b"
          (shouldBe
             (unifyConstraints [_F a _Text .~ _F _Integer b])
             (pure [a' .+-> _Integer, b' .+-> _Text]))
        it
          "F a a ~ F (Option b) (Option Integer)"
          (shouldBe
             (unifyConstraints [_F a a .~ _F (_Option b) (_Option _Integer)])
             (pure [a' .+-> _Option _Integer, b' .+-> _Integer]))
        it
          "(t ~ F a a, F a a ~ F (Option b) (Option Integer)) => t"
          (shouldBe
             (unifyAndSubstitute
                [t .~ _F a a, _F a a .~ _F (_Option b) (_Option _Integer)]
                t)
             (pure (solveType mempty (_F (_Option _Integer) (_Option _Integer))))))
  describe
    "Failing"
    (do it
          "Occurs check: F a b ~ a"
          (shouldBe
             (unifyConstraints [_F a b .~ a])
             (Left (pure (OccursCheckFail a' (_F a b)))))
        it
          "Kind mismatch: F a ~ b"
          (shouldBe
             (unifyConstraints [_F_partial a .~ b])
             (Left (pure (KindMismatch b' (_F_partial a)))))
        it
          "Constant mismatch: Integer ~ Text"
          (shouldBe
             (unifyConstraints [_Integer .~ _Text])
             (Left (pure (TypeMismatch (_Integer .~ _Text)))))
        it
          "Type mismatch: F a a ~ F (Option Text) (Option Integer)"
          (shouldBe
             (unifyConstraints [_F a a .~ _F (_Option _Text) (_Option _Integer)])
             (Left (pure (TypeMismatch (_Text .~ _Integer))))))

--------------------------------------------------------------------------------
-- Type variables

a' :: TypeVariable Generated
a' =
  TypeVariable
    {location = ExpressionCursor, prefix = NumberPrefix, index = 0, kind = TypeKind}

b' :: TypeVariable Generated
b' =
  TypeVariable
    {location = ExpressionCursor, prefix = NumberPrefix, index = 1, kind = TypeKind}

c' :: TypeVariable Generated
c' =
  TypeVariable
    {location = ExpressionCursor, prefix = NumberPrefix, index = 2, kind = TypeKind}

--------------------------------------------------------------------------------
-- Types of the variables

t :: Type Generated
t =
  VariableType
    TypeVariable
      {location = ExpressionCursor, prefix = NumberPrefix, index = 3, kind = TypeKind}

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
              , kind = FunKind TypeKind TypeKind
              }
      , argument = x2
      , kind = TypeKind
      }

_F_partial :: Type Generated -> Type Generated
_F_partial x1 =
  ApplyType
      TypeApplication
        { location = ExpressionCursor
        , function =
            ConstantType
              TypeConstant
                {location = ExpressionCursor, name = FunctionTypeName}
        , argument = x1
        , kind = FunKind TypeKind TypeKind
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
      , kind = TypeKind
      }

--------------------------------------------------------------------------------
-- Operators for easier reading

(.~) :: Type Generated -> Type Generated -> EqualityConstraint
(.~) x y =
  EqualityConstraint {location = ExpressionCursor, type1 = x, type2 = y}

(.+->) :: TypeVariable Generated -> Type Generated -> Substitution
(.+->) x y = Substitution {before = x, after = y}
