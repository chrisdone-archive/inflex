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

{-

TODO:

- Test order of implicits for a global.
- Test order of parameters.
- Test multiple class constraints.

-}

coarseGrained :: Spec
coarseGrained = do
  fromInteger_123
  lambda'dfromInteger_123

fromInteger_123 :: SpecWith ()
fromInteger_123 =
  it
    "fromInteger 123"
    (shouldBe
       (resolveText "" "fromInteger 123")
       (Right (IsResolved {thing = LambdaExpression (Lambda {location = ImplicitArgumentFor (ApplyFuncCursor ExpressionCursor), param = Param {location = ImplicitArgumentFor (ApplyFuncCursor ExpressionCursor), name = (), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}, body = ApplyExpression (Apply {location = ExpressionCursor, function = ApplyExpression (Apply {location = ImplicitlyApplicationOn (ApplyFuncCursor ExpressionCursor), function = GlobalExpression (Global {location = ApplyFuncCursor ExpressionCursor, name = FromIntegerGlobal, scheme = ResolvedScheme (ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = ApplyFuncCursor ExpressionCursor, name = FunctionTypeName}), argument = ConstantType (TypeConstant {location = ApplyFuncCursor ExpressionCursor, name = IntegerTypeName}), location = ApplyFuncCursor ExpressionCursor, kind = FunKind TypeKind TypeKind}), argument = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = ApplyFuncCursor ExpressionCursor, kind = TypeKind}))}), argument = VariableExpression (Variable {location = ImplicitArgumentFor (ApplyFuncCursor ExpressionCursor), name = DeBrujinIndex 0, typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = ApplyFuncCursor ExpressionCursor, name = FunctionTypeName}), argument = ConstantType (TypeConstant {location = ApplyFuncCursor ExpressionCursor, name = IntegerTypeName}), location = ApplyFuncCursor ExpressionCursor, kind = FunKind TypeKind TypeKind}), argument = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = ApplyFuncCursor ExpressionCursor, kind = TypeKind})}), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}), argument = LiteralExpression (NumberLiteral (Number {location = ApplyArgCursor ExpressionCursor, number = IntegerNumber 123, typ = ConstantType (TypeConstant {location = ApplyArgCursor ExpressionCursor, name = IntegerTypeName})})), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}), scheme = Scheme {location = ExpressionCursor, constraints = [ClassConstraint {className = FromIntegerClassName, typ = pure (VariableType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})), location = ApplyFuncCursor ExpressionCursor}], typ = VariableType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}, mappings = M.fromList [(ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 1, name = ""}, end = SourcePos {line = 1, column = 16, name = ""}}),(ApplyFuncCursor ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 1, name = ""}, end = SourcePos {line = 1, column = 12, name = ""}}),(ApplyArgCursor ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 13, name = ""}, end = SourcePos {line = 1, column = 16, name = ""}})]})))

lambda'dfromInteger_123 :: SpecWith ()
lambda'dfromInteger_123 =
  it
    "\\x->fromInteger x"
    (shouldBe
       (resolveText "" "\\x->fromInteger x")
       (Right (IsResolved {thing = LambdaExpression (Lambda {location = ImplicitArgumentFor (LambdaBodyCursor (ApplyFuncCursor ExpressionCursor)), param = Param {location = ImplicitArgumentFor (LambdaBodyCursor (ApplyFuncCursor ExpressionCursor)), name = (), typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = ExpressionCursor, name = FunctionTypeName}), argument = ConstantType (TypeConstant {location = LambdaBodyCursor (ApplyFuncCursor ExpressionCursor), name = IntegerTypeName}), location = ExpressionCursor, kind = FunKind TypeKind TypeKind}), argument = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = ExpressionCursor, kind = TypeKind})}, body = LambdaExpression (Lambda {location = ExpressionCursor, param = Param {location = LambdaParamCursor, name = (), typ = ConstantType (TypeConstant {location = LambdaBodyCursor (ApplyFuncCursor ExpressionCursor), name = IntegerTypeName})}, body = ApplyExpression (Apply {location = LambdaBodyCursor ExpressionCursor, function = ApplyExpression (Apply {location = ImplicitlyApplicationOn (LambdaBodyCursor (ApplyFuncCursor ExpressionCursor)), function = GlobalExpression (Global {location = LambdaBodyCursor (ApplyFuncCursor ExpressionCursor), name = FromIntegerGlobal, scheme = ResolvedScheme (ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = LambdaBodyCursor (ApplyFuncCursor ExpressionCursor), name = FunctionTypeName}), argument = ConstantType (TypeConstant {location = LambdaBodyCursor (ApplyFuncCursor ExpressionCursor), name = IntegerTypeName}), location = LambdaBodyCursor (ApplyFuncCursor ExpressionCursor), kind = FunKind TypeKind TypeKind}), argument = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = LambdaBodyCursor (ApplyFuncCursor ExpressionCursor), kind = TypeKind}))}), argument = VariableExpression (Variable {location = ImplicitArgumentFor (LambdaBodyCursor (ApplyFuncCursor ExpressionCursor)), name = DeBrujinIndex 1, typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = LambdaBodyCursor (ApplyFuncCursor ExpressionCursor), name = FunctionTypeName}), argument = ConstantType (TypeConstant {location = LambdaBodyCursor (ApplyFuncCursor ExpressionCursor), name = IntegerTypeName}), location = LambdaBodyCursor (ApplyFuncCursor ExpressionCursor), kind = FunKind TypeKind TypeKind}), argument = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = LambdaBodyCursor (ApplyFuncCursor ExpressionCursor), kind = TypeKind})}), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}), argument = VariableExpression (Variable {location = LambdaBodyCursor (ApplyArgCursor ExpressionCursor), name = DeBrujinIndex 0, typ = ConstantType (TypeConstant {location = LambdaBodyCursor (ApplyFuncCursor ExpressionCursor), name = IntegerTypeName})}), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}), typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = ExpressionCursor, name = FunctionTypeName}), argument = ConstantType (TypeConstant {location = LambdaBodyCursor (ApplyFuncCursor ExpressionCursor), name = IntegerTypeName}), location = ExpressionCursor, kind = FunKind TypeKind TypeKind}), argument = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = ExpressionCursor, kind = TypeKind})}), typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = ExpressionCursor, name = FunctionTypeName}), argument = ConstantType (TypeConstant {location = LambdaBodyCursor (ApplyFuncCursor ExpressionCursor), name = IntegerTypeName}), location = ExpressionCursor, kind = FunKind TypeKind TypeKind}), argument = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = ExpressionCursor, kind = TypeKind})}), scheme = Scheme {location = ExpressionCursor, constraints = [ClassConstraint {className = FromIntegerClassName, typ = pure (VariableType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})), location = LambdaBodyCursor (ApplyFuncCursor ExpressionCursor)}], typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = ExpressionCursor, name = FunctionTypeName}), argument = ConstantType (TypeConstant {location = LambdaBodyCursor (ApplyFuncCursor ExpressionCursor), name = IntegerTypeName}), location = ExpressionCursor, kind = FunKind TypeKind TypeKind}), argument = VariableType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = ExpressionCursor, kind = TypeKind})}, mappings = M.fromList [(ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 1, name = ""}, end = SourcePos {line = 1, column = 18, name = ""}}),(LambdaBodyCursor ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 5, name = ""}, end = SourcePos {line = 1, column = 18, name = ""}}),(LambdaBodyCursor (ApplyFuncCursor ExpressionCursor),SourceLocation {start = SourcePos {line = 1, column = 5, name = ""}, end = SourcePos {line = 1, column = 16, name = ""}}),(LambdaBodyCursor (ApplyArgCursor ExpressionCursor),SourceLocation {start = SourcePos {line = 1, column = 17, name = ""}, end = SourcePos {line = 1, column = 18, name = ""}}),(LambdaParamCursor,SourceLocation {start = SourcePos {line = 1, column = 2, name = ""}, end = SourcePos {line = 1, column = 3, name = ""}})]})))
