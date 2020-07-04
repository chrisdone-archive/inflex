{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- |

module ResolveSpec where

import qualified Data.List.NonEmpty as NE
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as M
import           Inflex.Generaliser
import           Inflex.Instances ()
import           Inflex.Resolver
import           Inflex.Solver as Solver
import           Inflex.Types
import           Test.Hspec

spec :: Spec
spec = do

  describe "Coarse-grained" coarseGrained

{-

Pretty much need row types to be able to have a productive expression
produce more than one type.

TODO: Test order of implicits for a global.
TODO: Test order of implicit parameters being correct.
TODO: Test multiple class constraints.

-}

coarseGrained :: Spec
coarseGrained = do
  fromInteger_123
  fromInteger_123_let
  fromInteger_123_DecimalSig
  fromDecimal_fails
  lambda'dfromInteger_123
  fromIntegerFromInteger_123

fromIntegerFromInteger_123 :: SpecWith ()
fromIntegerFromInteger_123 =
  it
    "fromInteger (fromInteger 123)"
    (shouldBe
       (resolveText "" "fromInteger (fromInteger 123)")
       (Right (IsResolved {thing = LambdaExpression (Lambda {location = ImplicitArgumentFor (ApplyFuncCursor ExpressionCursor), param = Param {location = ImplicitArgumentFor (ApplyFuncCursor ExpressionCursor), name = (), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}, body = ApplyExpression (Apply {location = ExpressionCursor, function = ApplyExpression (Apply {location = ImplicitlyApplicationOn (ApplyFuncCursor ExpressionCursor), function = GlobalExpression (Global {location = ApplyFuncCursor ExpressionCursor, name = FromIntegerGlobal, scheme = ResolvedScheme (ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = ApplyFuncCursor ExpressionCursor, name = FunctionTypeName}), argument = ConstantType (TypeConstant {location = ApplyFuncCursor ExpressionCursor, name = IntegerTypeName}), location = ApplyFuncCursor ExpressionCursor, kind = FunKind TypeKind TypeKind}), argument = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = ApplyFuncCursor ExpressionCursor, kind = TypeKind}))}), argument = VariableExpression (Variable {location = ImplicitArgumentFor (ApplyFuncCursor ExpressionCursor), name = DeBrujinIndex 0, typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = ApplyFuncCursor ExpressionCursor, name = FunctionTypeName}), argument = ConstantType (TypeConstant {location = ApplyFuncCursor ExpressionCursor, name = IntegerTypeName}), location = ApplyFuncCursor ExpressionCursor, kind = FunKind TypeKind TypeKind}), argument = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = ApplyFuncCursor ExpressionCursor, kind = TypeKind})}), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}), argument = ApplyExpression (Apply {location = ApplyArgCursor ExpressionCursor, function = ApplyExpression (Apply {location = ImplicitlyApplicationOn (ApplyArgCursor (ApplyFuncCursor ExpressionCursor)), function = GlobalExpression (Global {location = ApplyArgCursor (ApplyFuncCursor ExpressionCursor), name = FromIntegerGlobal, scheme = ResolvedScheme (ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = ApplyArgCursor (ApplyFuncCursor ExpressionCursor), name = FunctionTypeName}), argument = ConstantType (TypeConstant {location = ApplyArgCursor (ApplyFuncCursor ExpressionCursor), name = IntegerTypeName}), location = ApplyArgCursor (ApplyFuncCursor ExpressionCursor), kind = FunKind TypeKind TypeKind}), argument = ConstantType (TypeConstant {location = ApplyFuncCursor ExpressionCursor, name = IntegerTypeName}), location = ApplyArgCursor (ApplyFuncCursor ExpressionCursor), kind = TypeKind}))}), argument = GlobalExpression (Global {location = ImplicitArgumentFor (ApplyArgCursor (ApplyFuncCursor ExpressionCursor)), name = InstanceGlobal FromIntegerIntegerInstance, scheme = ResolvedScheme (ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = BuiltIn, name = IntegerTypeName}), argument = ConstantType (TypeConstant {location = BuiltIn, name = IntegerTypeName}), location = BuiltIn, kind = TypeKind}))}), typ = ConstantType (TypeConstant {location = ApplyFuncCursor ExpressionCursor, name = IntegerTypeName})}), argument = LiteralExpression (NumberLiteral (Number {location = ApplyArgCursor (ApplyArgCursor ExpressionCursor), number = IntegerNumber 123, typ = ConstantType (TypeConstant {location = ApplyArgCursor (ApplyArgCursor ExpressionCursor), name = IntegerTypeName})})), typ = ConstantType (TypeConstant {location = ApplyFuncCursor ExpressionCursor, name = IntegerTypeName})}), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}), scheme = Scheme {location = ExpressionCursor, constraints = [ClassConstraint {className = FromIntegerClassName, typ = pure (VariableType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})), location = ApplyFuncCursor ExpressionCursor}], typ = VariableType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}, mappings = M.fromList [(ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 1, name = ""}, end = SourcePos {line = 1, column = 29, name = ""}}),(ApplyFuncCursor ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 1, name = ""}, end = SourcePos {line = 1, column = 12, name = ""}}),(ApplyArgCursor ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 14, name = ""}, end = SourcePos {line = 1, column = 29, name = ""}}),(ApplyArgCursor (ApplyFuncCursor ExpressionCursor),SourceLocation {start = SourcePos {line = 1, column = 14, name = ""}, end = SourcePos {line = 1, column = 25, name = ""}}),(ApplyArgCursor (ApplyArgCursor ExpressionCursor),SourceLocation {start = SourcePos {line = 1, column = 26, name = ""}, end = SourcePos {line = 1, column = 29, name = ""}})]})))

fromInteger_123 :: SpecWith ()
fromInteger_123 =
  it
    "fromInteger 123"
    (shouldBe
       (resolveText "" "fromInteger 123")
       (Right (IsResolved {thing = LambdaExpression (Lambda {location = ImplicitArgumentFor (ApplyFuncCursor ExpressionCursor), param = Param {location = ImplicitArgumentFor (ApplyFuncCursor ExpressionCursor), name = (), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}, body = ApplyExpression (Apply {location = ExpressionCursor, function = ApplyExpression (Apply {location = ImplicitlyApplicationOn (ApplyFuncCursor ExpressionCursor), function = GlobalExpression (Global {location = ApplyFuncCursor ExpressionCursor, name = FromIntegerGlobal, scheme = ResolvedScheme (ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = ApplyFuncCursor ExpressionCursor, name = FunctionTypeName}), argument = ConstantType (TypeConstant {location = ApplyFuncCursor ExpressionCursor, name = IntegerTypeName}), location = ApplyFuncCursor ExpressionCursor, kind = FunKind TypeKind TypeKind}), argument = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = ApplyFuncCursor ExpressionCursor, kind = TypeKind}))}), argument = VariableExpression (Variable {location = ImplicitArgumentFor (ApplyFuncCursor ExpressionCursor), name = DeBrujinIndex 0, typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = ApplyFuncCursor ExpressionCursor, name = FunctionTypeName}), argument = ConstantType (TypeConstant {location = ApplyFuncCursor ExpressionCursor, name = IntegerTypeName}), location = ApplyFuncCursor ExpressionCursor, kind = FunKind TypeKind TypeKind}), argument = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = ApplyFuncCursor ExpressionCursor, kind = TypeKind})}), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}), argument = LiteralExpression (NumberLiteral (Number {location = ApplyArgCursor ExpressionCursor, number = IntegerNumber 123, typ = ConstantType (TypeConstant {location = ApplyArgCursor ExpressionCursor, name = IntegerTypeName})})), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}), scheme = Scheme {location = ExpressionCursor, constraints = [ClassConstraint {className = FromIntegerClassName, typ = pure (VariableType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})), location = ApplyFuncCursor ExpressionCursor}], typ = VariableType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}, mappings = M.fromList [(ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 1, name = ""}, end = SourcePos {line = 1, column = 16, name = ""}}),(ApplyFuncCursor ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 1, name = ""}, end = SourcePos {line = 1, column = 12, name = ""}}),(ApplyArgCursor ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 13, name = ""}, end = SourcePos {line = 1, column = 16, name = ""}})]})))

fromInteger_123_let :: SpecWith ()
fromInteger_123_let =
  it
    "let x = 123 in fromInteger x"
    (shouldBe
       (resolveText "" "let x = 123 in fromInteger x")
       (Right (IsResolved {thing = LambdaExpression (Lambda {location = ImplicitArgumentFor (LetBodyCursor (ApplyFuncCursor ExpressionCursor)), param = Param {location = ImplicitArgumentFor (LetBodyCursor (ApplyFuncCursor ExpressionCursor)), name = (), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}, body = LetExpression (Let {location = ExpressionCursor, binds = Bind {location = LetBindCursor (IndexInLet 0) ExpressionCursor, param = Param {location = LetBindCursor (IndexInLet 0) LambdaParamCursor, name = (), typ = ConstantType (TypeConstant {location = LetBindCursor (IndexInLet 0) ExpressionCursor, name = IntegerTypeName})}, value = LiteralExpression (NumberLiteral (Number {location = LetBindCursor (IndexInLet 0) ExpressionCursor, number = IntegerNumber 123, typ = ConstantType (TypeConstant {location = LetBindCursor (IndexInLet 0) ExpressionCursor, name = IntegerTypeName})})), typ = ConstantType (TypeConstant {location = LetBindCursor (IndexInLet 0) ExpressionCursor, name = IntegerTypeName})} :| [], body = ApplyExpression (Apply {location = LetBodyCursor ExpressionCursor, function = ApplyExpression (Apply {location = ImplicitlyApplicationOn (LetBodyCursor (ApplyFuncCursor ExpressionCursor)), function = GlobalExpression (Global {location = LetBodyCursor (ApplyFuncCursor ExpressionCursor), name = FromIntegerGlobal, scheme = ResolvedScheme (ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = LetBodyCursor (ApplyFuncCursor ExpressionCursor), name = FunctionTypeName}), argument = ConstantType (TypeConstant {location = LetBodyCursor (ApplyFuncCursor ExpressionCursor), name = IntegerTypeName}), location = LetBodyCursor (ApplyFuncCursor ExpressionCursor), kind = FunKind TypeKind TypeKind}), argument = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = LetBodyCursor (ApplyFuncCursor ExpressionCursor), kind = TypeKind}))}), argument = VariableExpression (Variable {location = ImplicitArgumentFor (LetBodyCursor (ApplyFuncCursor ExpressionCursor)), name = DeBrujinIndex (DeBrujinNesting 1), typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = LetBodyCursor (ApplyFuncCursor ExpressionCursor), name = FunctionTypeName}), argument = ConstantType (TypeConstant {location = LetBodyCursor (ApplyFuncCursor ExpressionCursor), name = IntegerTypeName}), location = LetBodyCursor (ApplyFuncCursor ExpressionCursor), kind = FunKind TypeKind TypeKind}), argument = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = LetBodyCursor (ApplyFuncCursor ExpressionCursor), kind = TypeKind})}), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}), argument = VariableExpression (Variable {location = LetBodyCursor (ApplyArgCursor ExpressionCursor), name = DeBrujinIndexOfLet (DeBrujinNesting 0) (IndexInLet 0), typ = ConstantType (TypeConstant {location = LetBindCursor (IndexInLet 0) ExpressionCursor, name = IntegerTypeName})}), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}), scheme = Scheme {location = ExpressionCursor, constraints = [ClassConstraint {className = FromIntegerClassName, typ = VariableType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}) :| [], location = LetBodyCursor (ApplyFuncCursor ExpressionCursor)}], typ = VariableType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}, mappings = M.fromList [(ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 1, name = ""}, end = SourcePos {line = 1, column = 29, name = ""}}),(LetBodyCursor ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 16, name = ""}, end = SourcePos {line = 1, column = 29, name = ""}}),(LetBodyCursor (ApplyFuncCursor ExpressionCursor),SourceLocation {start = SourcePos {line = 1, column = 16, name = ""}, end = SourcePos {line = 1, column = 27, name = ""}}),(LetBodyCursor (ApplyArgCursor ExpressionCursor),SourceLocation {start = SourcePos {line = 1, column = 28, name = ""}, end = SourcePos {line = 1, column = 29, name = ""}}),(LetBindCursor (IndexInLet 0) ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 9, name = ""}, end = SourcePos {line = 1, column = 12, name = ""}}),(LetBindCursor (IndexInLet 0) LambdaParamCursor,SourceLocation {start = SourcePos {line = 1, column = 5, name = ""}, end = SourcePos {line = 1, column = 6, name = ""}})]})))

lambda'dfromInteger_123 :: SpecWith ()
lambda'dfromInteger_123 =
  it
    "\\x->fromInteger x"
    (shouldBe
       (resolveText "" "\\x->fromInteger x")
       (Right (IsResolved {thing = LambdaExpression (Lambda {location = ImplicitArgumentFor (LambdaBodyCursor (ApplyFuncCursor ExpressionCursor)), param = Param {location = ImplicitArgumentFor (LambdaBodyCursor (ApplyFuncCursor ExpressionCursor)), name = (), typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = ExpressionCursor, name = FunctionTypeName}), argument = ConstantType (TypeConstant {location = LambdaBodyCursor (ApplyFuncCursor ExpressionCursor), name = IntegerTypeName}), location = ExpressionCursor, kind = FunKind TypeKind TypeKind}), argument = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = ExpressionCursor, kind = TypeKind})}, body = LambdaExpression (Lambda {location = ExpressionCursor, param = Param {location = LambdaParamCursor, name = (), typ = ConstantType (TypeConstant {location = LambdaBodyCursor (ApplyFuncCursor ExpressionCursor), name = IntegerTypeName})}, body = ApplyExpression (Apply {location = LambdaBodyCursor ExpressionCursor, function = ApplyExpression (Apply {location = ImplicitlyApplicationOn (LambdaBodyCursor (ApplyFuncCursor ExpressionCursor)), function = GlobalExpression (Global {location = LambdaBodyCursor (ApplyFuncCursor ExpressionCursor), name = FromIntegerGlobal, scheme = ResolvedScheme (ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = LambdaBodyCursor (ApplyFuncCursor ExpressionCursor), name = FunctionTypeName}), argument = ConstantType (TypeConstant {location = LambdaBodyCursor (ApplyFuncCursor ExpressionCursor), name = IntegerTypeName}), location = LambdaBodyCursor (ApplyFuncCursor ExpressionCursor), kind = FunKind TypeKind TypeKind}), argument = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = LambdaBodyCursor (ApplyFuncCursor ExpressionCursor), kind = TypeKind}))}), argument = VariableExpression (Variable {location = ImplicitArgumentFor (LambdaBodyCursor (ApplyFuncCursor ExpressionCursor)), name = DeBrujinIndex 1, typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = LambdaBodyCursor (ApplyFuncCursor ExpressionCursor), name = FunctionTypeName}), argument = ConstantType (TypeConstant {location = LambdaBodyCursor (ApplyFuncCursor ExpressionCursor), name = IntegerTypeName}), location = LambdaBodyCursor (ApplyFuncCursor ExpressionCursor), kind = FunKind TypeKind TypeKind}), argument = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = LambdaBodyCursor (ApplyFuncCursor ExpressionCursor), kind = TypeKind})}), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}), argument = VariableExpression (Variable {location = LambdaBodyCursor (ApplyArgCursor ExpressionCursor), name = DeBrujinIndex 0, typ = ConstantType (TypeConstant {location = LambdaBodyCursor (ApplyFuncCursor ExpressionCursor), name = IntegerTypeName})}), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}), typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = ExpressionCursor, name = FunctionTypeName}), argument = ConstantType (TypeConstant {location = LambdaBodyCursor (ApplyFuncCursor ExpressionCursor), name = IntegerTypeName}), location = ExpressionCursor, kind = FunKind TypeKind TypeKind}), argument = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = ExpressionCursor, kind = TypeKind})}), typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = ExpressionCursor, name = FunctionTypeName}), argument = ConstantType (TypeConstant {location = LambdaBodyCursor (ApplyFuncCursor ExpressionCursor), name = IntegerTypeName}), location = ExpressionCursor, kind = FunKind TypeKind TypeKind}), argument = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = ExpressionCursor, kind = TypeKind})}), scheme = Scheme {location = ExpressionCursor, constraints = [ClassConstraint {className = FromIntegerClassName, typ = pure (VariableType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})), location = LambdaBodyCursor (ApplyFuncCursor ExpressionCursor)}], typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = ExpressionCursor, name = FunctionTypeName}), argument = ConstantType (TypeConstant {location = LambdaBodyCursor (ApplyFuncCursor ExpressionCursor), name = IntegerTypeName}), location = ExpressionCursor, kind = FunKind TypeKind TypeKind}), argument = VariableType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = ExpressionCursor, kind = TypeKind})}, mappings = M.fromList [(ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 1, name = ""}, end = SourcePos {line = 1, column = 18, name = ""}}),(LambdaBodyCursor ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 5, name = ""}, end = SourcePos {line = 1, column = 18, name = ""}}),(LambdaBodyCursor (ApplyFuncCursor ExpressionCursor),SourceLocation {start = SourcePos {line = 1, column = 5, name = ""}, end = SourcePos {line = 1, column = 16, name = ""}}),(LambdaBodyCursor (ApplyArgCursor ExpressionCursor),SourceLocation {start = SourcePos {line = 1, column = 17, name = ""}, end = SourcePos {line = 1, column = 18, name = ""}}),(LambdaParamCursor,SourceLocation {start = SourcePos {line = 1, column = 2, name = ""}, end = SourcePos {line = 1, column = 3, name = ""}})]})))

fromInteger_123_DecimalSig :: Spec
fromInteger_123_DecimalSig =
  it "fromInteger 123 :: Decimal 2"
     (shouldBe
        (resolveText "" "fromInteger 123 :: Decimal 2")
        (Right (IsResolved {thing = ApplyExpression (Apply {location = ExpressionCursor, function = ApplyExpression (Apply {location = ImplicitlyApplicationOn (ApplyFuncCursor ExpressionCursor), function = GlobalExpression (Global {location = ApplyFuncCursor ExpressionCursor, name = FromIntegerGlobal, scheme = ResolvedScheme (ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = ApplyFuncCursor ExpressionCursor, name = FunctionTypeName}), argument = ConstantType (TypeConstant {location = ApplyFuncCursor ExpressionCursor, name = IntegerTypeName}), location = ApplyFuncCursor ExpressionCursor, kind = FunKind TypeKind TypeKind}), argument = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = SignatureCursor (TypeApplyCursor TypeCursor), name = DecimalTypeName}), argument = ConstantType (TypeConstant {location = SignatureCursor (TypeApplyCursor TypeCursor), name = NatTypeName 2}), location = SignatureCursor TypeCursor, kind = TypeKind}), location = ApplyFuncCursor ExpressionCursor, kind = TypeKind}))}), argument = GlobalExpression (Global {location = ImplicitArgumentFor (ApplyFuncCursor ExpressionCursor), name = InstanceGlobal (FromIntegerDecimalInstance 2), scheme = ResolvedScheme (ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = BuiltIn, name = IntegerTypeName}), argument = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = BuiltIn, name = DecimalTypeName}), argument = ConstantType (TypeConstant {location = BuiltIn, name = NatTypeName 2}), location = BuiltIn, kind = TypeKind}), location = BuiltIn, kind = TypeKind}))}), typ = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = SignatureCursor (TypeApplyCursor TypeCursor), name = DecimalTypeName}), argument = ConstantType (TypeConstant {location = SignatureCursor (TypeApplyCursor TypeCursor), name = NatTypeName 2}), location = SignatureCursor TypeCursor, kind = TypeKind})}), argument = LiteralExpression (NumberLiteral (Number {location = ApplyArgCursor ExpressionCursor, number = IntegerNumber 123, typ = ConstantType (TypeConstant {location = ApplyArgCursor ExpressionCursor, name = IntegerTypeName})})), typ = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = SignatureCursor (TypeApplyCursor TypeCursor), name = DecimalTypeName}), argument = ConstantType (TypeConstant {location = SignatureCursor (TypeApplyCursor TypeCursor), name = NatTypeName 2}), location = SignatureCursor TypeCursor, kind = TypeKind})}), scheme = Scheme {location = ExpressionCursor, constraints = [], typ = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = SignatureCursor (TypeApplyCursor TypeCursor), name = DecimalTypeName}), argument = ConstantType (TypeConstant {location = SignatureCursor (TypeApplyCursor TypeCursor), name = NatTypeName 2}), location = SignatureCursor TypeCursor, kind = TypeKind})}, mappings = M.fromList [(ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 1, name = ""}, end = SourcePos {line = 1, column = 16, name = ""}}),(ApplyFuncCursor ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 1, name = ""}, end = SourcePos {line = 1, column = 12, name = ""}}),(ApplyArgCursor ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 13, name = ""}, end = SourcePos {line = 1, column = 16, name = ""}}),(SignatureCursor TypeCursor,SourceLocation {start = SourcePos {line = 1, column = 20, name = ""}, end = SourcePos {line = 1, column = 29, name = ""}}),(SignatureCursor (TypeApplyCursor TypeCursor),SourceLocation {start = SourcePos {line = 1, column = 28, name = ""}, end = SourcePos {line = 1, column = 29, name = ""}})]})))

fromDecimal_fails :: Spec
fromDecimal_fails = do
  it
    "fromDecimal 12.0 :: Integer"
    (shouldBe
       (resolveText "" "fromDecimal 12.0 :: Integer")
       (Left
          (ResolverErrors
             (pure
                (NoInstanceForType
                   FromDecimalClassName
                   (ConstantType
                      (TypeConstant
                         { location = SignatureCursor TypeCursor
                         , name = IntegerTypeName
                         })))))))
  it
    "fromDecimal 12.52 :: Decimal 1"
    (shouldBe
       (resolveText "" "fromDecimal 12.52 :: Decimal 1")
       (Left
          (ResolverErrors
             (LiteralDecimalPrecisionMismatch
                (PrecisionMismatch
                   { supersetPlaces = 1
                   , subsetPlaces = 2
                   , constraint =
                       ClassConstraint
                         { className = FromDecimalClassName
                         , typ =
                             NE.fromList
                               [ ConstantType
                                   (TypeConstant
                                      { location =
                                          ApplyArgCursor ExpressionCursor
                                      , name = NatTypeName 2
                                      })
                               , ApplyType
                                   (TypeApplication
                                      { function =
                                          ConstantType
                                            (TypeConstant
                                               { location =
                                                   SignatureCursor
                                                     (TypeApplyCursor TypeCursor)
                                               , name = DecimalTypeName
                                               })
                                      , argument =
                                          ConstantType
                                            (TypeConstant
                                               { location =
                                                   SignatureCursor
                                                     (TypeApplyCursor TypeCursor)
                                               , name = NatTypeName 1
                                               })
                                      , location = SignatureCursor TypeCursor
                                      , kind = TypeKind
                                      })
                               ]
                         , location = ApplyFuncCursor ExpressionCursor
                         }
                   }) :|
              []))))
  it
    "fromDecimal (\\x -> x)"
    (shouldBe
       (resolveText "" "fromDecimal (\\x -> x)")
       (Left
          (GeneraliserErrored
             (SolverErrored
                (SolverErrors
                   (pure
                      (Solver.TypeMismatch
                         (EqualityConstraint
                            { type1 =
                                ConstantType
                                  (TypeConstant
                                     { location =
                                         ApplyFuncCursor ExpressionCursor
                                     , name = DecimalTypeName
                                     })
                            , type2 =
                                ApplyType
                                  (TypeApplication
                                     { function =
                                         ConstantType
                                           (TypeConstant
                                              { location =
                                                  ApplyArgCursor
                                                    ExpressionCursor
                                              , name = FunctionTypeName
                                              })
                                     , argument =
                                         VariableType
                                           (TypeVariable
                                              { location =
                                                  ApplyArgCursor
                                                    LambdaParamCursor
                                              , prefix = LambdaParameterPrefix
                                              , index = 2
                                              , kind = TypeKind
                                              })
                                     , location =
                                         ApplyArgCursor ExpressionCursor
                                     , kind = FunKind TypeKind TypeKind
                                     })
                            , location = ApplyFuncCursor ExpressionCursor
                            }))))))))
  it
    "fromInteger 5 :: Integer -> Integer"
    (shouldBe
       (resolveText "" "fromInteger 5 :: Integer -> Integer")
       (Left
          (ResolverErrors
             (NE.fromList
                [ NoInstanceForType
                    FromIntegerClassName
                    (ApplyType
                       (TypeApplication
                          { function =
                              ApplyType
                                (TypeApplication
                                   { function =
                                       ConstantType
                                         (TypeConstant
                                            { location =
                                                SignatureCursor
                                                  (TypeApplyCursor
                                                     (TypeApplyCursor TypeCursor))
                                            , name = FunctionTypeName
                                            })
                                   , argument =
                                       ConstantType
                                         (TypeConstant
                                            { location =
                                                SignatureCursor
                                                  (TypeApplyCursor
                                                     (TypeApplyCursor TypeCursor))
                                            , name = IntegerTypeName
                                            })
                                   , location =
                                       SignatureCursor
                                         (TypeApplyCursor TypeCursor)
                                   , kind = FunKind TypeKind TypeKind
                                   })
                          , argument =
                              ConstantType
                                (TypeConstant
                                   { location =
                                       SignatureCursor
                                         (TypeApplyCursor TypeCursor)
                                   , name = IntegerTypeName
                                   })
                          , location = SignatureCursor TypeCursor
                          , kind = TypeKind
                          }))
                ]))))
