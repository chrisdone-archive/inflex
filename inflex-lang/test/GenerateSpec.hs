{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Tests for generation of type constraints.

module GenerateSpec where

import           Data.Bifunctor
import           Data.List.NonEmpty (NonEmpty(..))
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
             { classes =
                 Seq.fromList
                   [ ClassConstraint
                       { className = FromIntegerClassName
                       , types =
                           VariableType
                             TypeVariable
                               { prefix = IntegeryPrefix
                               , index = 0
                               , location = ExpressionCursor
                               , kind = TypeKind
                               } :|
                           []
                       , location = ExpressionCursor
                       }
                   ]
             , thing =
                 LiteralExpression
                   (IntegerLiteral
                      (Integery
                         { location = ExpressionCursor
                         , integer = 123
                         , typ =
                             VariableType
                               TypeVariable
                                 { prefix = IntegeryPrefix
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
       (Right (HasConstraints {classes = Seq.fromList [ClassConstraint {className = FromIntegerClassName, types = VariableType (TypeVariable {location = LambdaBodyCursor ExpressionCursor, prefix = IntegeryPrefix, index = 1, kind = TypeKind}) :| [], location = LambdaBodyCursor ExpressionCursor}], equalities = mempty, thing = LambdaExpression (Lambda {location = ExpressionCursor, param = Param {location = LambdaParamCursor, name = (), typ = VariableType (TypeVariable {location = LambdaParamCursor, prefix = LambdaParameterPrefix, index = 0, kind = TypeKind})}, body = LiteralExpression (IntegerLiteral (Integery {location = LambdaBodyCursor ExpressionCursor, integer = 123, typ = VariableType (TypeVariable {location = LambdaBodyCursor ExpressionCursor, prefix = IntegeryPrefix, index = 1, kind = TypeKind})})), typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = ExpressionCursor, name = FunctionTypeName}), argument = VariableType (TypeVariable {location = LambdaParamCursor, prefix = LambdaParameterPrefix, index = 0, kind = TypeKind}), location = ExpressionCursor, kind = FunKind TypeKind TypeKind}), argument = VariableType (TypeVariable {location = LambdaBodyCursor ExpressionCursor, prefix = IntegeryPrefix, index = 1, kind = TypeKind}), location = ExpressionCursor, kind = TypeKind})}), mappings = mempty})))
  it
    "Apply"
    (do shouldBe
          (second
             (set hasConstraintsMappingsL mempty)
             (generateText "" "(\\x->x)123"))
          (Right (HasConstraints {classes = Seq.fromList [ClassConstraint {className = FromIntegerClassName, types = pure (VariableType (TypeVariable {location = ApplyArgCursor ExpressionCursor, prefix = IntegeryPrefix, index = 2, kind = TypeKind})), location = ApplyArgCursor ExpressionCursor}], equalities = Seq.fromList [EqualityConstraint {type1 = VariableType (TypeVariable {location = ApplyFuncCursor (LambdaBodyCursor ExpressionCursor), prefix = VariablePrefix, index = 1, kind = TypeKind}), type2 = VariableType (TypeVariable {location = ApplyFuncCursor LambdaParamCursor, prefix = LambdaParameterPrefix, index = 0, kind = TypeKind}), location = ApplyFuncCursor (LambdaBodyCursor ExpressionCursor)},EqualityConstraint {type1 = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = ApplyFuncCursor ExpressionCursor, name = FunctionTypeName}), argument = VariableType (TypeVariable {location = ApplyFuncCursor LambdaParamCursor, prefix = LambdaParameterPrefix, index = 0, kind = TypeKind}), location = ApplyFuncCursor ExpressionCursor, kind = FunKind TypeKind TypeKind}), argument = VariableType (TypeVariable {location = ApplyFuncCursor (LambdaBodyCursor ExpressionCursor), prefix = VariablePrefix, index = 1, kind = TypeKind}), location = ApplyFuncCursor ExpressionCursor, kind = TypeKind}), type2 = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = ExpressionCursor, name = FunctionTypeName}), argument = VariableType (TypeVariable {location = ApplyArgCursor ExpressionCursor, prefix = IntegeryPrefix, index = 2, kind = TypeKind}), location = ExpressionCursor, kind = FunKind TypeKind TypeKind}), argument = VariableType (TypeVariable {location = ApplyArgCursor ExpressionCursor, prefix = ApplyPrefix, index = 3, kind = TypeKind}), location = ApplyFuncCursor ExpressionCursor, kind = TypeKind}), location = ExpressionCursor}], thing = ApplyExpression (Apply {location = ExpressionCursor, function = LambdaExpression (Lambda {location = ApplyFuncCursor ExpressionCursor, param = Param {location = ApplyFuncCursor LambdaParamCursor, name = (), typ = VariableType (TypeVariable {location = ApplyFuncCursor LambdaParamCursor, prefix = LambdaParameterPrefix, index = 0, kind = TypeKind})}, body = VariableExpression (Variable {location = ApplyFuncCursor (LambdaBodyCursor ExpressionCursor), name = DeBrujinIndex 0, typ = VariableType (TypeVariable {location = ApplyFuncCursor (LambdaBodyCursor ExpressionCursor), prefix = VariablePrefix, index = 1, kind = TypeKind})}), typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = ApplyFuncCursor ExpressionCursor, name = FunctionTypeName}), argument = VariableType (TypeVariable {location = ApplyFuncCursor LambdaParamCursor, prefix = LambdaParameterPrefix, index = 0, kind = TypeKind}), location = ApplyFuncCursor ExpressionCursor, kind = FunKind TypeKind TypeKind}), argument = VariableType (TypeVariable {location = ApplyFuncCursor (LambdaBodyCursor ExpressionCursor), prefix = VariablePrefix, index = 1, kind = TypeKind}), location = ApplyFuncCursor ExpressionCursor, kind = TypeKind})}), argument = LiteralExpression (IntegerLiteral (Integery {location = ApplyArgCursor ExpressionCursor, integer = 123, typ = VariableType (TypeVariable {location = ApplyArgCursor ExpressionCursor, prefix = IntegeryPrefix, index = 2, kind = TypeKind})})), typ = VariableType (TypeVariable {location = ApplyArgCursor ExpressionCursor, prefix = ApplyPrefix, index = 3, kind = TypeKind})}), mappings = mempty})))
