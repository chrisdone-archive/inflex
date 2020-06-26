{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- |

module ResolveSpec where

import qualified Data.Map.Strict as M
import           Inflex.Resolver
import           Inflex.Instances ()
import           Inflex.Types
import           Test.Hspec

spec :: Spec
spec = do

  describe "Coarse-grained" coarseGrained

coarseGrained :: Spec
coarseGrained = do
  it
    "fromInteger 123"
    (shouldBe
       (resolveText "" "fromInteger 123")
       (Right (IsResolved {thing = LambdaExpression (Lambda {location = ImplicitArgumentFor (ApplyFuncCursor ExpressionCursor), param = Param {location = ImplicitArgumentFor (ApplyFuncCursor ExpressionCursor), name = (), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}, body = ApplyExpression (Apply {location = ExpressionCursor, function = ApplyExpression (Apply {location = ImplicitlyApplicationOn (ApplyFuncCursor ExpressionCursor), function = GlobalExpression (Global {location = ApplyFuncCursor ExpressionCursor, name = FromIntegerGlobal, scheme = ResolvedScheme (ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = ApplyFuncCursor ExpressionCursor, name = FunctionTypeName}), argument = ConstantType (TypeConstant {location = ApplyFuncCursor ExpressionCursor, name = IntegerTypeName}), location = ApplyFuncCursor ExpressionCursor, kind = FunKind TypeKind TypeKind}), argument = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = ApplyFuncCursor ExpressionCursor, kind = TypeKind}))}), argument = VariableExpression (Variable {location = ImplicitArgumentFor (ApplyFuncCursor ExpressionCursor), name = DeBrujinIndex 0, typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = ApplyFuncCursor ExpressionCursor, name = FunctionTypeName}), argument = ConstantType (TypeConstant {location = ApplyFuncCursor ExpressionCursor, name = IntegerTypeName}), location = ApplyFuncCursor ExpressionCursor, kind = FunKind TypeKind TypeKind}), argument = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = ApplyFuncCursor ExpressionCursor, kind = TypeKind})}), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}), argument = LiteralExpression (NumberLiteral (Number {location = ApplyArgCursor ExpressionCursor, number = IntegerNumber 123, typ = ConstantType (TypeConstant {location = ApplyArgCursor ExpressionCursor, name = IntegerTypeName})})), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}), scheme = Scheme {location = ExpressionCursor, constraints = [ClassConstraint {className = FromIntegerClassName, typ = pure (VariableType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})), location = ApplyFuncCursor ExpressionCursor}], typ = VariableType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}, mappings = M.fromList [(ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 1, name = ""}, end = SourcePos {line = 1, column = 16, name = ""}}),(ApplyFuncCursor ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 1, name = ""}, end = SourcePos {line = 1, column = 12, name = ""}}),(ApplyArgCursor ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 13, name = ""}, end = SourcePos {line = 1, column = 16, name = ""}})]})))
