{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Check defaulting works as expected, and fails as expected.

module DefaultSpec where

import           Data.Bifunctor
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Map.Strict (Map)
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import           Inflex.Defaulter
import           Inflex.Types
import           Test.Hspec

defaultText' :: (e~ ()) =>
     Map Hash (Either e (Scheme Polymorphic))
  -> FilePath
  -> Text
  -> Either (ResolverDefaulterError e) Cell
defaultText' = defaultText

spec :: Spec
spec = do
  spec_1
  spec_1_0
  spec_1_plus_1_0
  spec_lambda
  spec_table_nums

spec_1 :: SpecWith ()
spec_1 =
  it
    "1"
    (shouldBe
       (defaultText' mempty "" "0")
       (Right (Cell {location = BuiltIn, defaulted = ApplyExpression (Apply {location = AutoInsertedForDefaulterCursor, function = LambdaExpression (Lambda {location = ImplicitArgumentFor BuiltIn, param = Param {location = ImplicitArgumentFor BuiltIn, name = (), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}, body = ApplyExpression (Apply {location = BuiltIn, function = ApplyExpression (Apply {location = ImplicitlyApplicationOn BuiltIn, function = GlobalExpression (Global {location = BuiltIn, name = FromIntegerGlobal, scheme = ResolvedScheme (ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = BuiltIn, name = FunctionTypeName}), argument = ConstantType (TypeConstant {location = BuiltIn, name = IntegerTypeName}), location = BuiltIn, kind = FunKind TypeKind TypeKind}), argument = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = BuiltIn, kind = TypeKind}))}), argument = VariableExpression (Variable {location = ImplicitArgumentFor BuiltIn, name = DeBrujinIndex (DeBrujinNesting 0), typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = BuiltIn, name = FunctionTypeName}), argument = ConstantType (TypeConstant {location = BuiltIn, name = IntegerTypeName}), location = BuiltIn, kind = FunKind TypeKind TypeKind}), argument = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = BuiltIn, kind = TypeKind})}), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}), argument = LiteralExpression (NumberLiteral (Number {location = ExpressionCursor, number = IntegerNumber 0, typ = ConstantType (TypeConstant {location = ExpressionCursor, name = IntegerTypeName})})), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}), argument = GlobalExpression (Global {location = AutoInsertedForDefaulterCursor, name = InstanceGlobal FromIntegerIntegerInstance, scheme = ResolvedScheme (ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = BuiltIn, name = FunctionTypeName}), argument = ConstantType (TypeConstant {location = BuiltIn, name = IntegerTypeName}), location = BuiltIn, kind = FunKind TypeKind TypeKind}), argument = ConstantType (TypeConstant {location = BuiltIn, name = IntegerTypeName}), location = BuiltIn, kind = TypeKind}))}), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}), scheme = Scheme {location = BuiltIn, constraints = [], typ = ConstantType (TypeConstant {location = DefaultedCursor, name = IntegerTypeName})}, defaultedClassConstraints = Seq.fromList [Default {classConstraintDefaulted = ClassConstraint {className = FromIntegerClassName, typ = ConstantType (TypeConstant {location = DefaultedCursor, name = IntegerTypeName}) :| [], location = BuiltIn}, classConstraintOriginal = ClassConstraint {className = FromIntegerClassName, typ = VariableType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}) :| [], location = BuiltIn}, instanceName = FromIntegerIntegerInstance}], ambiguousClassConstraints = mempty})))

spec_table_nums :: SpecWith ()
spec_table_nums =
  it
    "table nums"
    (do shouldBe
          (second (\Cell{scheme} -> scheme) (defaultText'
              mempty
              ""
              ("let f = r:r.y+r.x in f")))
          (Right (Scheme {location = ExpressionCursor, constraints = [ClassConstraint {className = AddOpClassName, typ = VariableType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}) :| [], location = LetBindCursor (IndexInLet 0) (LambdaBodyCursor (InfixOpCursor ExpressionCursor))}], typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = LetBindCursor (IndexInLet 0) ExpressionCursor, name = FunctionTypeName}), argument = RecordType (RowType (TypeRow {location = LetBindCursor (IndexInLet 0) (LambdaBodyCursor (InfixLeftCursor ExpressionCursor)), typeVariable = Just (TypeVariable {location = (), prefix = (), index = 1, kind = RowKind}), fields = [Field {location = LetBindCursor (IndexInLet 0) (LambdaBodyCursor (InfixLeftCursor ExpressionCursor)), name = FieldName {unFieldName = "y"}, typ = VariableType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})},Field {location = LetBindCursor (IndexInLet 0) (LambdaBodyCursor (InfixRightCursor ExpressionCursor)), name = FieldName {unFieldName = "x"}, typ = VariableType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}]})), location = LetBindCursor (IndexInLet 0) ExpressionCursor, kind = FunKind TypeKind TypeKind}), argument = VariableType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = LetBindCursor (IndexInLet 0) ExpressionCursor, kind = TypeKind})}))
        shouldBe
          (second (\Cell{scheme} -> scheme) (defaultText'
              mempty
              ""
              ("let f = r:r.y+r.x in f({x:3,y:2.0})")))
          (Right
             (Scheme
                { location = ExpressionCursor
                , constraints = []
                , typ =
                    ApplyType
                      (TypeApplication
                         { function =
                             ConstantType
                               (TypeConstant
                                  {location = DefaultedCursor, name = DecimalTypeName})
                         , argument =
                             ConstantType
                               (TypeConstant
                                  { location =
                                      LetBodyCursor
                                        (ApplyArgCursor
                                           (RecordFieldCursor
                                              (FieldName {unFieldName = "y"})
                                              (RowFieldExpression ExpressionCursor)))
                                  , name = NatTypeName 1
                                  })
                         , location = DefaultedCursor
                         , kind = TypeKind
                         })
                }))
        shouldBe
          (second (\Cell{scheme} -> scheme) (defaultText'
              mempty
              ""
              ("map(r:r.y+r.x,[{x:3,y:2.0}])")))
          (Right
             (Scheme
                { location = ExpressionCursor
                , constraints = []
                , typ =
                    ArrayType
                      (ApplyType
                         (TypeApplication
                            { function =
                                ConstantType
                                  (TypeConstant
                                     {location = DefaultedCursor, name = DecimalTypeName})
                            , argument =
                                ConstantType
                                  (TypeConstant
                                     { location =
                                         ApplyArgCursor
                                           (ArrayElementCursor
                                              0
                                              (RecordFieldCursor
                                                 (FieldName {unFieldName = "y"})
                                                 (RowFieldExpression ExpressionCursor)))
                                     , name = NatTypeName 1
                                     })
                            , location = DefaultedCursor
                            , kind = TypeKind
                            }))
                })))

spec_1_0 :: SpecWith ()
spec_1_0 =
  it
    "1.0"
    (shouldBe
       (defaultText' mempty "" "1.0")
       (Right (Cell {location = BuiltIn, defaulted = ApplyExpression (Apply {location = AutoInsertedForDefaulterCursor, function = LambdaExpression (Lambda {location = ImplicitArgumentFor BuiltIn, param = Param {location = ImplicitArgumentFor BuiltIn, name = (), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}, body = ApplyExpression (Apply {location = BuiltIn, function = ApplyExpression (Apply {location = ImplicitlyApplicationOn BuiltIn, function = GlobalExpression (Global {location = BuiltIn, name = FromDecimalGlobal, scheme = ResolvedScheme (ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = BuiltIn, name = FunctionTypeName}), argument = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = BuiltIn, name = DecimalTypeName}), argument = ConstantType (TypeConstant {location = ExpressionCursor, name = NatTypeName 1}), location = BuiltIn, kind = TypeKind}), location = BuiltIn, kind = FunKind TypeKind TypeKind}), argument = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = BuiltIn, kind = TypeKind}))}), argument = VariableExpression (Variable {location = ImplicitArgumentFor BuiltIn, name = DeBrujinIndex (DeBrujinNesting 0), typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = BuiltIn, name = FunctionTypeName}), argument = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = BuiltIn, name = DecimalTypeName}), argument = ConstantType (TypeConstant {location = ExpressionCursor, name = NatTypeName 1}), location = BuiltIn, kind = TypeKind}), location = BuiltIn, kind = FunKind TypeKind TypeKind}), argument = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = BuiltIn, kind = TypeKind})}), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}), argument = LiteralExpression (NumberLiteral (Number {location = ExpressionCursor, number = DecimalNumber (Decimal {places = 1, integer = 10}), typ = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = ExpressionCursor, name = DecimalTypeName}), argument = ConstantType (TypeConstant {location = ExpressionCursor, name = NatTypeName 1}), location = ExpressionCursor, kind = TypeKind})})), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}), argument = GlobalExpression (Global {location = AutoInsertedForDefaulterCursor, name = InstanceGlobal (FromDecimalDecimalInstance (FromDecimalInstance {supersetPlaces = 1, subsetPlaces = 1})), scheme = ResolvedScheme (ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = BuiltIn, name = FunctionTypeName}), argument = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = BuiltIn, name = DecimalTypeName}), argument = ConstantType (TypeConstant {location = BuiltIn, name = NatTypeName 1}), location = BuiltIn, kind = TypeKind}), location = BuiltIn, kind = FunKind TypeKind TypeKind}), argument = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = BuiltIn, name = DecimalTypeName}), argument = ConstantType (TypeConstant {location = BuiltIn, name = NatTypeName 1}), location = BuiltIn, kind = TypeKind}), location = BuiltIn, kind = TypeKind}))}), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}), scheme = Scheme {location = BuiltIn, constraints = [], typ = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = DefaultedCursor, name = DecimalTypeName}), argument = ConstantType (TypeConstant {location = ExpressionCursor, name = NatTypeName 1}), location = DefaultedCursor, kind = TypeKind})}, defaultedClassConstraints = Seq.fromList [Default {classConstraintDefaulted = ClassConstraint {className = FromDecimalClassName, typ = ConstantType (TypeConstant {location = ExpressionCursor, name = NatTypeName 1}) :| [ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = DefaultedCursor, name = DecimalTypeName}), argument = ConstantType (TypeConstant {location = ExpressionCursor, name = NatTypeName 1}), location = DefaultedCursor, kind = TypeKind})], location = BuiltIn}, classConstraintOriginal = ClassConstraint {className = FromDecimalClassName, typ = ConstantType (TypeConstant {location = ExpressionCursor, name = NatTypeName 1}) :| [VariableType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})], location = BuiltIn}, instanceName = FromDecimalDecimalInstance (FromDecimalInstance {supersetPlaces = 1, subsetPlaces = 1})}], ambiguousClassConstraints = mempty})))

spec_1_plus_1_0 :: SpecWith ()
spec_1_plus_1_0 =
  it
    "1 + 1.0"
    (shouldBe
       (defaultText' mempty "" "1 + 1.0")
       (Right (Cell {location = ExpressionCursor, defaulted = ApplyExpression (Apply {location = AutoInsertedForDefaulterCursor, function = LambdaExpression (Lambda {location = ImplicitArgumentFor BuiltIn, param = Param {location = ImplicitArgumentFor BuiltIn, name = (), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}, body = ApplyExpression (Apply {location = AutoInsertedForDefaulterCursor, function = LambdaExpression (Lambda {location = ImplicitArgumentFor BuiltIn, param = Param {location = ImplicitArgumentFor BuiltIn, name = (), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}, body = ApplyExpression (Apply {location = AutoInsertedForDefaulterCursor, function = LambdaExpression (Lambda {location = ImplicitArgumentFor (InfixOpCursor ExpressionCursor), param = Param {location = ImplicitArgumentFor (InfixOpCursor ExpressionCursor), name = (), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}, body = InfixExpression (Infix {location = ExpressionCursor, global = ApplyExpression (Apply {location = ImplicitlyApplicationOn (InfixOpCursor ExpressionCursor), function = GlobalExpression (Global {location = InfixOpCursor ExpressionCursor, name = NumericBinOpGlobal AddOp, scheme = ResolvedScheme (ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = BuiltIn, name = FunctionTypeName}), argument = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = BuiltIn, kind = FunKind TypeKind TypeKind}), argument = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = BuiltIn, name = FunctionTypeName}), argument = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = BuiltIn, kind = FunKind TypeKind TypeKind}), argument = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = BuiltIn, kind = TypeKind}), location = BuiltIn, kind = TypeKind}))}), argument = VariableExpression (Variable {location = ImplicitArgumentFor (InfixOpCursor ExpressionCursor), name = DeBrujinIndex (DeBrujinNesting 0), typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = BuiltIn, name = FunctionTypeName}), argument = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = BuiltIn, kind = FunKind TypeKind TypeKind}), argument = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = BuiltIn, name = FunctionTypeName}), argument = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = BuiltIn, kind = FunKind TypeKind TypeKind}), argument = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = BuiltIn, kind = TypeKind}), location = BuiltIn, kind = TypeKind})}), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}), left = ApplyExpression (Apply {location = BuiltIn, function = ApplyExpression (Apply {location = ImplicitlyApplicationOn BuiltIn, function = GlobalExpression (Global {location = BuiltIn, name = FromIntegerGlobal, scheme = ResolvedScheme (ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = BuiltIn, name = FunctionTypeName}), argument = ConstantType (TypeConstant {location = BuiltIn, name = IntegerTypeName}), location = BuiltIn, kind = FunKind TypeKind TypeKind}), argument = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = BuiltIn, kind = TypeKind}))}), argument = VariableExpression (Variable {location = ImplicitArgumentFor BuiltIn, name = DeBrujinIndex (DeBrujinNesting 1), typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = BuiltIn, name = FunctionTypeName}), argument = ConstantType (TypeConstant {location = BuiltIn, name = IntegerTypeName}), location = BuiltIn, kind = FunKind TypeKind TypeKind}), argument = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = BuiltIn, kind = TypeKind})}), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}), argument = LiteralExpression (NumberLiteral (Number {location = InfixLeftCursor ExpressionCursor, number = IntegerNumber 1, typ = ConstantType (TypeConstant {location = InfixLeftCursor ExpressionCursor, name = IntegerTypeName})})), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}), right = ApplyExpression (Apply {location = BuiltIn, function = ApplyExpression (Apply {location = ImplicitlyApplicationOn BuiltIn, function = GlobalExpression (Global {location = BuiltIn, name = FromDecimalGlobal, scheme = ResolvedScheme (ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = BuiltIn, name = FunctionTypeName}), argument = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = BuiltIn, name = DecimalTypeName}), argument = ConstantType (TypeConstant {location = InfixRightCursor ExpressionCursor, name = NatTypeName 1}), location = BuiltIn, kind = TypeKind}), location = BuiltIn, kind = FunKind TypeKind TypeKind}), argument = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = BuiltIn, kind = TypeKind}))}), argument = VariableExpression (Variable {location = ImplicitArgumentFor BuiltIn, name = DeBrujinIndex (DeBrujinNesting 2), typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = BuiltIn, name = FunctionTypeName}), argument = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = BuiltIn, name = DecimalTypeName}), argument = ConstantType (TypeConstant {location = InfixRightCursor ExpressionCursor, name = NatTypeName 1}), location = BuiltIn, kind = TypeKind}), location = BuiltIn, kind = FunKind TypeKind TypeKind}), argument = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = BuiltIn, kind = TypeKind})}), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}), argument = LiteralExpression (NumberLiteral (Number {location = InfixRightCursor ExpressionCursor, number = DecimalNumber (Decimal {places = 1, integer = 10}), typ = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = InfixRightCursor ExpressionCursor, name = DecimalTypeName}), argument = ConstantType (TypeConstant {location = InfixRightCursor ExpressionCursor, name = NatTypeName 1}), location = InfixRightCursor ExpressionCursor, kind = TypeKind})})), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}), argument = GlobalExpression (Global {location = AutoInsertedForDefaulterCursor, name = InstanceGlobal (DecimalOpInstance 1 AddOp), scheme = ResolvedScheme (ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = BuiltIn, name = FunctionTypeName}), argument = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = BuiltIn, name = DecimalTypeName}), argument = ConstantType (TypeConstant {location = BuiltIn, name = NatTypeName 1}), location = BuiltIn, kind = TypeKind}), location = BuiltIn, kind = FunKind TypeKind TypeKind}), argument = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = BuiltIn, name = FunctionTypeName}), argument = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = BuiltIn, name = DecimalTypeName}), argument = ConstantType (TypeConstant {location = BuiltIn, name = NatTypeName 1}), location = BuiltIn, kind = TypeKind}), location = BuiltIn, kind = FunKind TypeKind TypeKind}), argument = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = BuiltIn, name = DecimalTypeName}), argument = ConstantType (TypeConstant {location = BuiltIn, name = NatTypeName 1}), location = BuiltIn, kind = TypeKind}), location = BuiltIn, kind = TypeKind}), location = BuiltIn, kind = TypeKind}))}), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}), argument = GlobalExpression (Global {location = AutoInsertedForDefaulterCursor, name = InstanceGlobal (FromIntegerDecimalInstance 1), scheme = ResolvedScheme (ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = BuiltIn, name = FunctionTypeName}), argument = ConstantType (TypeConstant {location = BuiltIn, name = IntegerTypeName}), location = BuiltIn, kind = FunKind TypeKind TypeKind}), argument = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = BuiltIn, name = DecimalTypeName}), argument = ConstantType (TypeConstant {location = BuiltIn, name = NatTypeName 1}), location = BuiltIn, kind = TypeKind}), location = BuiltIn, kind = TypeKind}))}), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}), argument = GlobalExpression (Global {location = AutoInsertedForDefaulterCursor, name = InstanceGlobal (FromDecimalDecimalInstance (FromDecimalInstance {supersetPlaces = 1, subsetPlaces = 1})), scheme = ResolvedScheme (ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = BuiltIn, name = FunctionTypeName}), argument = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = BuiltIn, name = DecimalTypeName}), argument = ConstantType (TypeConstant {location = BuiltIn, name = NatTypeName 1}), location = BuiltIn, kind = TypeKind}), location = BuiltIn, kind = FunKind TypeKind TypeKind}), argument = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = BuiltIn, name = DecimalTypeName}), argument = ConstantType (TypeConstant {location = BuiltIn, name = NatTypeName 1}), location = BuiltIn, kind = TypeKind}), location = BuiltIn, kind = TypeKind}))}), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}), scheme = Scheme {location = ExpressionCursor, constraints = [], typ = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = DefaultedCursor, name = DecimalTypeName}), argument = ConstantType (TypeConstant {location = InfixRightCursor ExpressionCursor, name = NatTypeName 1}), location = DefaultedCursor, kind = TypeKind})}, defaultedClassConstraints = Seq.fromList [Default {classConstraintDefaulted = ClassConstraint {className = FromDecimalClassName, typ = ConstantType (TypeConstant {location = InfixRightCursor ExpressionCursor, name = NatTypeName 1}) :| [ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = DefaultedCursor, name = DecimalTypeName}), argument = ConstantType (TypeConstant {location = InfixRightCursor ExpressionCursor, name = NatTypeName 1}), location = DefaultedCursor, kind = TypeKind})], location = BuiltIn}, classConstraintOriginal = ClassConstraint {className = FromDecimalClassName, typ = ConstantType (TypeConstant {location = InfixRightCursor ExpressionCursor, name = NatTypeName 1}) :| [VariableType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})], location = BuiltIn}, instanceName = FromDecimalDecimalInstance (FromDecimalInstance {supersetPlaces = 1, subsetPlaces = 1})},Default {classConstraintDefaulted = ClassConstraint {className = FromIntegerClassName, typ = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = DefaultedCursor, name = DecimalTypeName}), argument = ConstantType (TypeConstant {location = InfixRightCursor ExpressionCursor, name = NatTypeName 1}), location = DefaultedCursor, kind = TypeKind}) :| [], location = BuiltIn}, classConstraintOriginal = ClassConstraint {className = FromIntegerClassName, typ = VariableType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}) :| [], location = BuiltIn}, instanceName = FromIntegerDecimalInstance 1},Default {classConstraintDefaulted = ClassConstraint {className = AddOpClassName, typ = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = DefaultedCursor, name = DecimalTypeName}), argument = ConstantType (TypeConstant {location = InfixRightCursor ExpressionCursor, name = NatTypeName 1}), location = DefaultedCursor, kind = TypeKind}) :| [], location = InfixOpCursor ExpressionCursor}, classConstraintOriginal = ClassConstraint {className = AddOpClassName, typ = VariableType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}) :| [], location = InfixOpCursor ExpressionCursor}, instanceName = DecimalOpInstance 1 AddOp}], ambiguousClassConstraints = mempty})))

spec_lambda :: SpecWith ()
spec_lambda =
  it
    "x : x + x"
    (shouldBe
       (defaultText' mempty "" "x : x + x")
       (Right (Cell {location = ExpressionCursor, defaulted = LambdaExpression (Lambda {location = ImplicitArgumentFor (LambdaBodyCursor (InfixOpCursor ExpressionCursor)), param = Param {location = ImplicitArgumentFor (LambdaBodyCursor (InfixOpCursor ExpressionCursor)), name = (), typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = ExpressionCursor, name = FunctionTypeName}), argument = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = ExpressionCursor, kind = FunKind TypeKind TypeKind}), argument = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = ExpressionCursor, kind = TypeKind})}, body = LambdaExpression (Lambda {location = ExpressionCursor, param = Param {location = LambdaParamCursor, name = (), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}, body = InfixExpression (Infix {location = LambdaBodyCursor ExpressionCursor, global = ApplyExpression (Apply {location = ImplicitlyApplicationOn (LambdaBodyCursor (InfixOpCursor ExpressionCursor)), function = GlobalExpression (Global {location = LambdaBodyCursor (InfixOpCursor ExpressionCursor), name = NumericBinOpGlobal AddOp, scheme = ResolvedScheme (ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = BuiltIn, name = FunctionTypeName}), argument = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = BuiltIn, kind = FunKind TypeKind TypeKind}), argument = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = BuiltIn, name = FunctionTypeName}), argument = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = BuiltIn, kind = FunKind TypeKind TypeKind}), argument = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = BuiltIn, kind = TypeKind}), location = BuiltIn, kind = TypeKind}))}), argument = VariableExpression (Variable {location = ImplicitArgumentFor (LambdaBodyCursor (InfixOpCursor ExpressionCursor)), name = DeBrujinIndex (DeBrujinNesting 1), typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = BuiltIn, name = FunctionTypeName}), argument = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = BuiltIn, kind = FunKind TypeKind TypeKind}), argument = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = BuiltIn, name = FunctionTypeName}), argument = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = BuiltIn, kind = FunKind TypeKind TypeKind}), argument = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = BuiltIn, kind = TypeKind}), location = BuiltIn, kind = TypeKind})}), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}), left = VariableExpression (Variable {location = LambdaBodyCursor (InfixLeftCursor ExpressionCursor), name = DeBrujinIndex (DeBrujinNesting 0), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}), right = VariableExpression (Variable {location = LambdaBodyCursor (InfixRightCursor ExpressionCursor), name = DeBrujinIndex (DeBrujinNesting 0), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}), typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = ExpressionCursor, name = FunctionTypeName}), argument = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = ExpressionCursor, kind = FunKind TypeKind TypeKind}), argument = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = ExpressionCursor, kind = TypeKind})}), typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = ExpressionCursor, name = FunctionTypeName}), argument = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = ExpressionCursor, kind = FunKind TypeKind TypeKind}), argument = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = ExpressionCursor, kind = TypeKind})}), scheme = Scheme {location = ExpressionCursor, constraints = [ClassConstraint {className = AddOpClassName, typ = VariableType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}) :| [], location = LambdaBodyCursor (InfixOpCursor ExpressionCursor)}], typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = ExpressionCursor, name = FunctionTypeName}), argument = VariableType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = ExpressionCursor, kind = FunKind TypeKind TypeKind}), argument = VariableType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = ExpressionCursor, kind = TypeKind})}, defaultedClassConstraints = mempty, ambiguousClassConstraints = mempty})))
