{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Tests for generation of type constraints.

module GenerateSpec where

import           Data.Bifunctor
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
    (shouldBe
       (second (set hasConstraintsMappingsL mempty) (generateText "" "123"))
       (Right
          (HasConstraints
             { thing =
                 LiteralExpression
                   (NumberLiteral
                      (Number
                         { location = ExpressionCursor
                         , number = IntegerNumber 123
                         , typ =
                             VariableType
                               TypeVariable
                                 { prefix = NumberPrefix
                                 , index = 0
                                 , location = ExpressionCursor
                                 , kind = TypeKind
                                 }
                         }))
             , mappings = mempty
             , equalities = mempty
             })))
  it
    "Lambda"
    (shouldBe
       (second (set hasConstraintsMappingsL mempty) (generateText "" "\\x->123"))
       (Right (HasConstraints {equalities = mempty, thing = LambdaExpression (Lambda {location = ExpressionCursor, param = Param {location = LambdaParamCursor, name = (), typ = VariableType (TypeVariable {location = LambdaParamCursor, prefix = LambdaParameterPrefix, index = 0, kind = TypeKind})}, body = LiteralExpression (NumberLiteral (Number {location = LambdaBodyCursor ExpressionCursor, number = IntegerNumber 123, typ = VariableType (TypeVariable {location = LambdaBodyCursor ExpressionCursor, prefix = NumberPrefix, index = 1, kind = TypeKind})})), typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = ExpressionCursor, name = FunctionTypeName}), argument = VariableType (TypeVariable {location = LambdaParamCursor, prefix = LambdaParameterPrefix, index = 0, kind = TypeKind}), location = ExpressionCursor, kind = FunKind TypeKind TypeKind}), argument = VariableType (TypeVariable {location = LambdaBodyCursor ExpressionCursor, prefix = NumberPrefix, index = 1, kind = TypeKind}), location = ExpressionCursor, kind = TypeKind})}), mappings = mempty})))
  it
    "Apply"
    (do shouldBe
          (second
             (set hasConstraintsMappingsL mempty)
             (generateText "" "(\\x->x)123"))
          (Right (HasConstraints {equalities = Seq.fromList [EqualityConstraint {type1 = VariableType (TypeVariable {location = ApplyFuncCursor (LambdaBodyCursor ExpressionCursor), prefix = VariablePrefix, index = 1, kind = TypeKind}), type2 = VariableType (TypeVariable {location = ApplyFuncCursor LambdaParamCursor, prefix = LambdaParameterPrefix, index = 0, kind = TypeKind}), location = ApplyFuncCursor (LambdaBodyCursor ExpressionCursor)},EqualityConstraint {type1 = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = ApplyFuncCursor ExpressionCursor, name = FunctionTypeName}), argument = VariableType (TypeVariable {location = ApplyFuncCursor LambdaParamCursor, prefix = LambdaParameterPrefix, index = 0, kind = TypeKind}), location = ApplyFuncCursor ExpressionCursor, kind = FunKind TypeKind TypeKind}), argument = VariableType (TypeVariable {location = ApplyFuncCursor (LambdaBodyCursor ExpressionCursor), prefix = VariablePrefix, index = 1, kind = TypeKind}), location = ApplyFuncCursor ExpressionCursor, kind = TypeKind}), type2 = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = ExpressionCursor, name = FunctionTypeName}), argument = VariableType (TypeVariable {location = ApplyArgCursor ExpressionCursor, prefix = NumberPrefix, index = 2, kind = TypeKind}), location = ExpressionCursor, kind = FunKind TypeKind TypeKind}), argument = VariableType (TypeVariable {location = ApplyArgCursor ExpressionCursor, prefix = ApplyPrefix, index = 3, kind = TypeKind}), location = ApplyFuncCursor ExpressionCursor, kind = TypeKind}), location = ExpressionCursor}], thing = ApplyExpression (Apply {location = ExpressionCursor, function = LambdaExpression (Lambda {location = ApplyFuncCursor ExpressionCursor, param = Param {location = ApplyFuncCursor LambdaParamCursor, name = (), typ = VariableType (TypeVariable {location = ApplyFuncCursor LambdaParamCursor, prefix = LambdaParameterPrefix, index = 0, kind = TypeKind})}, body = VariableExpression (Variable {location = ApplyFuncCursor (LambdaBodyCursor ExpressionCursor), name = DeBrujinIndex 0, typ = VariableType (TypeVariable {location = ApplyFuncCursor (LambdaBodyCursor ExpressionCursor), prefix = VariablePrefix, index = 1, kind = TypeKind})}), typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = ApplyFuncCursor ExpressionCursor, name = FunctionTypeName}), argument = VariableType (TypeVariable {location = ApplyFuncCursor LambdaParamCursor, prefix = LambdaParameterPrefix, index = 0, kind = TypeKind}), location = ApplyFuncCursor ExpressionCursor, kind = FunKind TypeKind TypeKind}), argument = VariableType (TypeVariable {location = ApplyFuncCursor (LambdaBodyCursor ExpressionCursor), prefix = VariablePrefix, index = 1, kind = TypeKind}), location = ApplyFuncCursor ExpressionCursor, kind = TypeKind})}), argument = LiteralExpression (NumberLiteral (Number {location = ApplyArgCursor ExpressionCursor, number = IntegerNumber 123, typ = VariableType (TypeVariable {location = ApplyArgCursor ExpressionCursor, prefix = NumberPrefix, index = 2, kind = TypeKind})})), typ = VariableType (TypeVariable {location = ApplyArgCursor ExpressionCursor, prefix = ApplyPrefix, index = 3, kind = TypeKind})}), mappings = mempty})))
