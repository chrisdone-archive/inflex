{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Tests for generation of type constraints.

module GenerateSpec where

import           Data.Bifunctor
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import           Inflex.Generator
import           Inflex.Instances ()
import           Inflex.Types
import           Optics
import           Test.Hspec

spec :: Spec
spec = do
  it
    "Literal"
    (do shouldBe
          (second (set hasConstraintsMappingsL mempty) (generateText "" "123"))
          (Right (HasConstraints {equalities = Seq.fromList [], thing = LiteralExpression (NumberLiteral (Number {location = ExpressionCursor, number = IntegerNumber 123, typ = ConstantType (TypeConstant {location = ExpressionCursor, name = IntegerTypeName})})), mappings = M.fromList []}))
        shouldBe
          (second (set hasConstraintsMappingsL mempty) (generateText "" "123.0"))
          (Right (HasConstraints {equalities = Seq.fromList [], thing = LiteralExpression (NumberLiteral (Number {location = ExpressionCursor, number = DecimalNumber (Decimal {places = 1, integer = 1230}), typ = ConstantType (TypeConstant {location = ExpressionCursor, name = DecimalTypeName 1})})), mappings = M.fromList []}))
        shouldBe
          (second (set hasConstraintsMappingsL mempty) (generateText "" "0.00"))
          (Right (HasConstraints {equalities = Seq.fromList [], thing = LiteralExpression (NumberLiteral (Number {location = ExpressionCursor, number = DecimalNumber (Decimal {places = 2, integer = 0}), typ = ConstantType (TypeConstant {location = ExpressionCursor, name = DecimalTypeName 2})})), mappings = M.fromList []})))
  it
    "Lambda"
    (shouldBe
       (second (set hasConstraintsMappingsL mempty) (generateText "" "\\x->123"))
       (Right (HasConstraints {equalities = Seq.fromList [], thing = LambdaExpression (Lambda {location = ExpressionCursor, param = Param {location = LambdaParamCursor, name = (), typ = VariableType (TypeVariable {location = LambdaParamCursor, prefix = LambdaParameterPrefix, index = 0, kind = TypeKind})}, body = LiteralExpression (NumberLiteral (Number {location = LambdaBodyCursor ExpressionCursor, number = IntegerNumber 123, typ = ConstantType (TypeConstant {location = LambdaBodyCursor ExpressionCursor, name = IntegerTypeName})})), typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = ExpressionCursor, name = FunctionTypeName}), argument = VariableType (TypeVariable {location = LambdaParamCursor, prefix = LambdaParameterPrefix, index = 0, kind = TypeKind}), location = ExpressionCursor, kind = FunKind TypeKind TypeKind}), argument = ConstantType (TypeConstant {location = LambdaBodyCursor ExpressionCursor, name = IntegerTypeName}), location = ExpressionCursor, kind = TypeKind})}), mappings = M.fromList []})))
  it
    "Apply"
    (do shouldBe
          (second
             (set hasConstraintsMappingsL mempty)
             (generateText "" "(\\x->x)123"))
          (Right (HasConstraints {equalities = Seq.fromList [EqualityConstraint {type1 = VariableType (TypeVariable {location = ApplyFuncCursor (LambdaBodyCursor ExpressionCursor), prefix = VariablePrefix, index = 1, kind = TypeKind}), type2 = VariableType (TypeVariable {location = ApplyFuncCursor LambdaParamCursor, prefix = LambdaParameterPrefix, index = 0, kind = TypeKind}), location = ApplyFuncCursor (LambdaBodyCursor ExpressionCursor)},EqualityConstraint {type1 = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = ApplyFuncCursor ExpressionCursor, name = FunctionTypeName}), argument = VariableType (TypeVariable {location = ApplyFuncCursor LambdaParamCursor, prefix = LambdaParameterPrefix, index = 0, kind = TypeKind}), location = ApplyFuncCursor ExpressionCursor, kind = FunKind TypeKind TypeKind}), argument = VariableType (TypeVariable {location = ApplyFuncCursor (LambdaBodyCursor ExpressionCursor), prefix = VariablePrefix, index = 1, kind = TypeKind}), location = ApplyFuncCursor ExpressionCursor, kind = TypeKind}), type2 = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = ExpressionCursor, name = FunctionTypeName}), argument = ConstantType (TypeConstant {location = ApplyArgCursor ExpressionCursor, name = IntegerTypeName}), location = ExpressionCursor, kind = FunKind TypeKind TypeKind}), argument = VariableType (TypeVariable {location = ApplyArgCursor ExpressionCursor, prefix = ApplyPrefix, index = 2, kind = TypeKind}), location = ApplyFuncCursor ExpressionCursor, kind = TypeKind}), location = ExpressionCursor}], thing = ApplyExpression (Apply {location = ExpressionCursor, function = LambdaExpression (Lambda {location = ApplyFuncCursor ExpressionCursor, param = Param {location = ApplyFuncCursor LambdaParamCursor, name = (), typ = VariableType (TypeVariable {location = ApplyFuncCursor LambdaParamCursor, prefix = LambdaParameterPrefix, index = 0, kind = TypeKind})}, body = VariableExpression (Variable {location = ApplyFuncCursor (LambdaBodyCursor ExpressionCursor), name = DeBrujinIndex 0, typ = VariableType (TypeVariable {location = ApplyFuncCursor (LambdaBodyCursor ExpressionCursor), prefix = VariablePrefix, index = 1, kind = TypeKind})}), typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = ApplyFuncCursor ExpressionCursor, name = FunctionTypeName}), argument = VariableType (TypeVariable {location = ApplyFuncCursor LambdaParamCursor, prefix = LambdaParameterPrefix, index = 0, kind = TypeKind}), location = ApplyFuncCursor ExpressionCursor, kind = FunKind TypeKind TypeKind}), argument = VariableType (TypeVariable {location = ApplyFuncCursor (LambdaBodyCursor ExpressionCursor), prefix = VariablePrefix, index = 1, kind = TypeKind}), location = ApplyFuncCursor ExpressionCursor, kind = TypeKind})}), argument = LiteralExpression (NumberLiteral (Number {location = ApplyArgCursor ExpressionCursor, number = IntegerNumber 123, typ = ConstantType (TypeConstant {location = ApplyArgCursor ExpressionCursor, name = IntegerTypeName})})), typ = VariableType (TypeVariable {location = ApplyArgCursor ExpressionCursor, prefix = ApplyPrefix, index = 2, kind = TypeKind})}), mappings = M.fromList []})))
