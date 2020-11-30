{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- |

module SolverSpec where

import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as M
import           Data.Sequence (Seq)
import           Data.Text (Text)
import           Inflex.Instances ()
import           Inflex.Solver
import           Inflex.Type
import           Inflex.Types
import           Test.Hspec

solveText' :: (e ~ ()) =>
     M.Map Hash (Either e (Scheme Polymorphic))
  -> FilePath
  -> Text
  -> Either (GenerateSolveError e) (IsSolved (Expression Solved))
solveText' = solveText

unifyConstraints' ::
     Seq EqualityConstraint
  -> Either (NonEmpty SolveError) (Seq Substitution)
unifyConstraints' = runSolver . unifyConstraints

unifyAndSubstitute' ::
     Seq EqualityConstraint
  -> Type Generated
  -> Either (NonEmpty SolveError) (Type Solved)
unifyAndSubstitute' x = runSolver . unifyAndSubstitute x

spec :: Spec
spec = do
  describe "Fine-grained" fineGrained
  describe "Coarse-grained" coarseGrained

--------------------------------------------------------------------------------
-- Coarse-grained tests

coarseGrained :: Spec
coarseGrained = do
  arrays
  variants
  describe
    "Successful"
    (do it "r:r.x"
           (shouldBe
           (fmap (expressionType . Inflex.Solver.thing) (solveText' mempty "" "r:(r.x)"))
                 (Right
                    (ApplyType
                       (TypeApplication
                          { function =
                              ApplyType
                                (TypeApplication
                                   { function =
                                       ConstantType
                                         (TypeConstant
                                            {location = ExpressionCursor, name = FunctionTypeName})
                                   , argument =
                                       RecordType
                                         (RowType
                                            (TypeRow
                                               { location = LambdaBodyCursor ExpressionCursor
                                               , typeVariable =
                                                   Just
                                                     (TypeVariable
                                                        { location =
                                                            LambdaBodyCursor ExpressionCursor
                                                        , prefix = RowVarPrefix
                                                        , index = 1
                                                        , kind = RowKind
                                                        })
                                               , fields =
                                                   [ Field
                                                       { location =
                                                           LambdaBodyCursor ExpressionCursor
                                                       , name = FieldName {unFieldName = "x"}
                                                       , typ =
                                                           VariableType
                                                             (TypeVariable
                                                                { location =
                                                                    LambdaBodyCursor
                                                                      ExpressionCursor
                                                                , prefix = FieldTypePrefix
                                                                , index = 3
                                                                , kind = TypeKind
                                                                })
                                                       }
                                                   ]
                                               }))
                                   , location = ExpressionCursor
                                   , kind = FunKind TypeKind TypeKind
                                   })
                          , argument =
                              VariableType
                                (TypeVariable
                                   { location = LambdaBodyCursor ExpressionCursor
                                   , prefix = FieldTypePrefix
                                   , index = 3
                                   , kind = TypeKind
                                   })
                          , location = ExpressionCursor
                          , kind = TypeKind
                          }))))
        it
          "[{x:1},{x:1}]"
          (shouldBe
             (fmap Inflex.Solver.thing (solveText' mempty "" "[{x:1},{x:1}]"))
             (Right (ArrayExpression (Array {expressions = [RecordExpression (Record {fields = [FieldE {name = FieldName {unFieldName = "x"}, expression = ApplyExpression (Apply {location = BuiltIn, function = GlobalExpression (Global {location = BuiltIn, name = FromIntegerGlobal, scheme = SolvedScheme (Scheme {location = BuiltIn, constraints = [ClassConstraint {className = FromIntegerClassName, typ = VariableType (TypeVariable {location = ArrayElementCursor 1 (RecordFieldCursor (FieldName {unFieldName = "x"}) (RowFieldExpression ExpressionCursor)), prefix = ApplyPrefix, index = 4, kind = TypeKind}) :| [], location = BuiltIn}], typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = BuiltIn, name = FunctionTypeName}), argument = ConstantType (TypeConstant {location = BuiltIn, name = IntegerTypeName}), location = BuiltIn, kind = FunKind TypeKind TypeKind}), argument = VariableType (TypeVariable {location = ArrayElementCursor 1 (RecordFieldCursor (FieldName {unFieldName = "x"}) (RowFieldExpression ExpressionCursor)), prefix = ApplyPrefix, index = 4, kind = TypeKind}), location = BuiltIn, kind = TypeKind})})}), argument = LiteralExpression (NumberLiteral (Number {location = ArrayElementCursor 0 (RecordFieldCursor (FieldName {unFieldName = "x"}) (RowFieldExpression ExpressionCursor)), number = IntegerNumber 1, typ = ConstantType (TypeConstant {location = ArrayElementCursor 0 (RecordFieldCursor (FieldName {unFieldName = "x"}) (RowFieldExpression ExpressionCursor)), name = IntegerTypeName})})), typ = VariableType (TypeVariable {location = ArrayElementCursor 1 (RecordFieldCursor (FieldName {unFieldName = "x"}) (RowFieldExpression ExpressionCursor)), prefix = ApplyPrefix, index = 4, kind = TypeKind})}), location = ArrayElementCursor 0 (RecordFieldCursor (FieldName {unFieldName = "x"}) TypeCursor)}], location = ArrayElementCursor 0 ExpressionCursor, typ = RecordType (RowType (TypeRow {location = ArrayElementCursor 0 ExpressionCursor, typeVariable = Nothing, fields = [Field {location = ArrayElementCursor 0 (RecordFieldCursor (FieldName {unFieldName = "x"}) TypeCursor), name = FieldName {unFieldName = "x"}, typ = VariableType (TypeVariable {location = ArrayElementCursor 1 (RecordFieldCursor (FieldName {unFieldName = "x"}) (RowFieldExpression ExpressionCursor)), prefix = ApplyPrefix, index = 4, kind = TypeKind})}]}))}),RecordExpression (Record {fields = [FieldE {name = FieldName {unFieldName = "x"}, expression = ApplyExpression (Apply {location = BuiltIn, function = GlobalExpression (Global {location = BuiltIn, name = FromIntegerGlobal, scheme = SolvedScheme (Scheme {location = BuiltIn, constraints = [ClassConstraint {className = FromIntegerClassName, typ = VariableType (TypeVariable {location = ArrayElementCursor 1 (RecordFieldCursor (FieldName {unFieldName = "x"}) (RowFieldExpression ExpressionCursor)), prefix = ApplyPrefix, index = 4, kind = TypeKind}) :| [], location = BuiltIn}], typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = BuiltIn, name = FunctionTypeName}), argument = ConstantType (TypeConstant {location = BuiltIn, name = IntegerTypeName}), location = BuiltIn, kind = FunKind TypeKind TypeKind}), argument = VariableType (TypeVariable {location = ArrayElementCursor 1 (RecordFieldCursor (FieldName {unFieldName = "x"}) (RowFieldExpression ExpressionCursor)), prefix = ApplyPrefix, index = 4, kind = TypeKind}), location = BuiltIn, kind = TypeKind})})}), argument = LiteralExpression (NumberLiteral (Number {location = ArrayElementCursor 1 (RecordFieldCursor (FieldName {unFieldName = "x"}) (RowFieldExpression ExpressionCursor)), number = IntegerNumber 1, typ = ConstantType (TypeConstant {location = ArrayElementCursor 1 (RecordFieldCursor (FieldName {unFieldName = "x"}) (RowFieldExpression ExpressionCursor)), name = IntegerTypeName})})), typ = VariableType (TypeVariable {location = ArrayElementCursor 1 (RecordFieldCursor (FieldName {unFieldName = "x"}) (RowFieldExpression ExpressionCursor)), prefix = ApplyPrefix, index = 4, kind = TypeKind})}), location = ArrayElementCursor 1 (RecordFieldCursor (FieldName {unFieldName = "x"}) TypeCursor)}], location = ArrayElementCursor 1 ExpressionCursor, typ = RecordType (RowType (TypeRow {location = ArrayElementCursor 1 ExpressionCursor, typeVariable = Nothing, fields = [Field {location = ArrayElementCursor 1 (RecordFieldCursor (FieldName {unFieldName = "x"}) TypeCursor), name = FieldName {unFieldName = "x"}, typ = VariableType (TypeVariable {location = ArrayElementCursor 1 (RecordFieldCursor (FieldName {unFieldName = "x"}) (RowFieldExpression ExpressionCursor)), prefix = ApplyPrefix, index = 4, kind = TypeKind})}]}))})], typ = ArrayType (RecordType (RowType (TypeRow {location = ArrayElementCursor 0 ExpressionCursor, typeVariable = Nothing, fields = [Field {location = ArrayElementCursor 0 (RecordFieldCursor (FieldName {unFieldName = "x"}) TypeCursor), name = FieldName {unFieldName = "x"}, typ = VariableType (TypeVariable {location = ArrayElementCursor 1 (RecordFieldCursor (FieldName {unFieldName = "x"}) (RowFieldExpression ExpressionCursor)), prefix = ApplyPrefix, index = 4, kind = TypeKind})}]}))), location = ExpressionCursor}))))
        it
          "{x:1}"
          (shouldBe
             (fmap Inflex.Solver.thing (solveText' mempty "" "{x:1}"))
             (Right (RecordExpression (Record {fields = [FieldE {name = FieldName {unFieldName = "x"}, expression = ApplyExpression (Apply {location = BuiltIn, function = GlobalExpression (Global {location = BuiltIn, name = FromIntegerGlobal, scheme = SolvedScheme (Scheme {location = BuiltIn, constraints = [ClassConstraint {className = FromIntegerClassName, typ = VariableType (TypeVariable {location = RecordFieldCursor (FieldName {unFieldName = "x"}) (RowFieldExpression ExpressionCursor), prefix = ApplyPrefix, index = 1, kind = TypeKind}) :| [], location = BuiltIn}], typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = BuiltIn, name = FunctionTypeName}), argument = ConstantType (TypeConstant {location = BuiltIn, name = IntegerTypeName}), location = BuiltIn, kind = FunKind TypeKind TypeKind}), argument = VariableType (TypeVariable {location = RecordFieldCursor (FieldName {unFieldName = "x"}) (RowFieldExpression ExpressionCursor), prefix = ApplyPrefix, index = 1, kind = TypeKind}), location = BuiltIn, kind = TypeKind})})}), argument = LiteralExpression (NumberLiteral (Number {location = RecordFieldCursor (FieldName {unFieldName = "x"}) (RowFieldExpression ExpressionCursor), number = IntegerNumber 1, typ = ConstantType (TypeConstant {location = RecordFieldCursor (FieldName {unFieldName = "x"}) (RowFieldExpression ExpressionCursor), name = IntegerTypeName})})), typ = VariableType (TypeVariable {location = RecordFieldCursor (FieldName {unFieldName = "x"}) (RowFieldExpression ExpressionCursor), prefix = ApplyPrefix, index = 1, kind = TypeKind})}), location = RecordFieldCursor (FieldName {unFieldName = "x"}) TypeCursor}], location = ExpressionCursor, typ = RecordType (RowType (TypeRow {location = ExpressionCursor, typeVariable = Nothing, fields = [Field {location = RecordFieldCursor (FieldName {unFieldName = "x"}) TypeCursor, name = FieldName {unFieldName = "x"}, typ = VariableType (TypeVariable {location = RecordFieldCursor (FieldName {unFieldName = "x"}) (RowFieldExpression ExpressionCursor), prefix = ApplyPrefix, index = 1, kind = TypeKind})}]}))}))))
        it
          "{x:1}.x"
          (shouldBe
             (fmap Inflex.Solver.thing (solveText' mempty "" "{x:1}.x"))
             (Right (PropExpression (Prop {expression = RecordExpression (Record {fields = [FieldE {name = FieldName {unFieldName = "x"}, expression = ApplyExpression (Apply {location = BuiltIn, function = GlobalExpression (Global {location = BuiltIn, name = FromIntegerGlobal, scheme = SolvedScheme (Scheme {location = BuiltIn, constraints = [ClassConstraint {className = FromIntegerClassName, typ = VariableType (TypeVariable {location = PropExpressionCursor (RecordFieldCursor (FieldName {unFieldName = "x"}) (RowFieldExpression ExpressionCursor)), prefix = ApplyPrefix, index = 2, kind = TypeKind}) :| [], location = BuiltIn}], typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = BuiltIn, name = FunctionTypeName}), argument = ConstantType (TypeConstant {location = BuiltIn, name = IntegerTypeName}), location = BuiltIn, kind = FunKind TypeKind TypeKind}), argument = VariableType (TypeVariable {location = PropExpressionCursor (RecordFieldCursor (FieldName {unFieldName = "x"}) (RowFieldExpression ExpressionCursor)), prefix = ApplyPrefix, index = 2, kind = TypeKind}), location = BuiltIn, kind = TypeKind})})}), argument = LiteralExpression (NumberLiteral (Number {location = PropExpressionCursor (RecordFieldCursor (FieldName {unFieldName = "x"}) (RowFieldExpression ExpressionCursor)), number = IntegerNumber 1, typ = ConstantType (TypeConstant {location = PropExpressionCursor (RecordFieldCursor (FieldName {unFieldName = "x"}) (RowFieldExpression ExpressionCursor)), name = IntegerTypeName})})), typ = VariableType (TypeVariable {location = PropExpressionCursor (RecordFieldCursor (FieldName {unFieldName = "x"}) (RowFieldExpression ExpressionCursor)), prefix = ApplyPrefix, index = 2, kind = TypeKind})}), location = PropExpressionCursor (RecordFieldCursor (FieldName {unFieldName = "x"}) TypeCursor)}], location = PropExpressionCursor ExpressionCursor, typ = RecordType (RowType (TypeRow {location = PropExpressionCursor ExpressionCursor, typeVariable = Nothing, fields = [Field {location = PropExpressionCursor (RecordFieldCursor (FieldName {unFieldName = "x"}) TypeCursor), name = FieldName {unFieldName = "x"}, typ = VariableType (TypeVariable {location = PropExpressionCursor (RecordFieldCursor (FieldName {unFieldName = "x"}) (RowFieldExpression ExpressionCursor)), prefix = ApplyPrefix, index = 2, kind = TypeKind})}]}))}), name = FieldName {unFieldName = "x"}, typ = VariableType (TypeVariable {location = PropExpressionCursor (RecordFieldCursor (FieldName {unFieldName = "x"}) (RowFieldExpression ExpressionCursor)), prefix = ApplyPrefix, index = 2, kind = TypeKind}), location = ExpressionCursor}))))
        it
          "123"
          (shouldBe
             (solveText' mempty "" "(123::Integer)")
             (Right (IsSolved {thing = LiteralExpression (NumberLiteral (Number {location = ExpressionCursor, number = IntegerNumber 123, typ = ConstantType (TypeConstant {location = ExpressionCursor, name = IntegerTypeName})})), mappings = M.fromList [(ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 2, name = ""}, end = SourcePos {line = 1, column = 5, name = ""}}),(SignatureCursor TypeCursor,SourceLocation {start = SourcePos {line = 1, column = 7, name = ""}, end = SourcePos {line = 1, column = 14, name = ""}})]})))
        it
          "(x:x)123"
          (shouldBe
             (solveText' mempty "" "(x:x)(123::Integer)")
             (Right (IsSolved {thing = ApplyExpression (Apply {location = ExpressionCursor, function = LambdaExpression (Lambda {location = ApplyFuncCursor ExpressionCursor, param = Param {location = ApplyFuncCursor LambdaParamCursor, name = (), typ = ConstantType (TypeConstant {location = ApplyArgCursor ExpressionCursor, name = IntegerTypeName})}, body = VariableExpression (Variable {location = ApplyFuncCursor (LambdaBodyCursor ExpressionCursor), name = DeBrujinIndex (DeBrujinNesting 0), typ = ConstantType (TypeConstant {location = ApplyArgCursor ExpressionCursor, name = IntegerTypeName})}), typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = ApplyFuncCursor ExpressionCursor, name = FunctionTypeName}), argument = ConstantType (TypeConstant {location = ApplyArgCursor ExpressionCursor, name = IntegerTypeName}), location = ApplyFuncCursor ExpressionCursor, kind = FunKind TypeKind TypeKind}), argument = ConstantType (TypeConstant {location = ApplyArgCursor ExpressionCursor, name = IntegerTypeName}), location = ApplyFuncCursor ExpressionCursor, kind = TypeKind})}), argument = LiteralExpression (NumberLiteral (Number {location = ApplyArgCursor ExpressionCursor, number = IntegerNumber 123, typ = ConstantType (TypeConstant {location = ApplyArgCursor ExpressionCursor, name = IntegerTypeName})})), typ = ConstantType (TypeConstant {location = ApplyArgCursor ExpressionCursor, name = IntegerTypeName})}), mappings = M.fromList [(ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 7, name = ""}, end = SourcePos {line = 1, column = 10, name = ""}}),(ApplyFuncCursor ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 2, name = ""}, end = SourcePos {line = 1, column = 5, name = ""}}),(ApplyFuncCursor (LambdaBodyCursor ExpressionCursor),SourceLocation {start = SourcePos {line = 1, column = 4, name = ""}, end = SourcePos {line = 1, column = 5, name = ""}}),(ApplyFuncCursor LambdaParamCursor,SourceLocation {start = SourcePos {line = 1, column = 2, name = ""}, end = SourcePos {line = 1, column = 3, name = ""}}),(ApplyArgCursor ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 7, name = ""}, end = SourcePos {line = 1, column = 10, name = ""}}),(ApplyArgCursor (SignatureCursor TypeCursor),SourceLocation {start = SourcePos {line = 1, column = 12, name = ""}, end = SourcePos {line = 1, column = 19, name = ""}})]}))))
  erroneous

erroneous :: SpecWith ()
erroneous =
  describe
    "Erroneous"
  (do
     it
       "{x:1}.y"
       (shouldBe
          (solveText' mempty "" "{x:1}.y")
          (Left (SolverErrors (RowMismatch (TypeRow {location = ExpressionCursor, typeVariable = Just (TypeVariable {location = ExpressionCursor, prefix = RowVarPrefix, index = 0, kind = RowKind}), fields = [Field {location = ExpressionCursor, name = FieldName {unFieldName = "y"}, typ = VariableType (TypeVariable {location = ExpressionCursor, prefix = FieldTypePrefix, index = 3, kind = TypeKind})}]}) (TypeRow {location = PropExpressionCursor ExpressionCursor, typeVariable = Nothing, fields = [Field {location = PropExpressionCursor (RecordFieldCursor (FieldName {unFieldName = "x"}) TypeCursor), name = FieldName {unFieldName = "x"}, typ = VariableType (TypeVariable {location = PropExpressionCursor (RecordFieldCursor (FieldName {unFieldName = "x"}) (RowFieldExpression ExpressionCursor)), prefix = ApplyPrefix, index = 2, kind = TypeKind})}]}) :| []))))
     it
       "[{x:1},{y:1}]"
       (shouldBe
          (fmap Inflex.Solver.thing (solveText' mempty "" "[{x:1},{y:1}]"))
          (Left (SolverErrors (RowMismatch (TypeRow {location = ArrayElementCursor 0 ExpressionCursor, typeVariable = Nothing, fields = [Field {location = ArrayElementCursor 0 (RecordFieldCursor (FieldName {unFieldName = "x"}) TypeCursor), name = FieldName {unFieldName = "x"}, typ = VariableType (TypeVariable {location = ArrayElementCursor 0 (RecordFieldCursor (FieldName {unFieldName = "x"}) (RowFieldExpression ExpressionCursor)), prefix = ApplyPrefix, index = 2, kind = TypeKind})}]}) (TypeRow {location = ArrayElementCursor 1 ExpressionCursor, typeVariable = Nothing, fields = [Field {location = ArrayElementCursor 1 (RecordFieldCursor (FieldName {unFieldName = "y"}) TypeCursor), name = FieldName {unFieldName = "y"}, typ = VariableType (TypeVariable {location = ArrayElementCursor 1 (RecordFieldCursor (FieldName {unFieldName = "y"}) (RowFieldExpression ExpressionCursor)), prefix = ApplyPrefix, index = 4, kind = TypeKind})}]}) :| []))))
     it
       "[{x:1},{y:{}]"
       (shouldBe
          (fmap Inflex.Solver.thing (solveText' mempty "" "[{x:1},{y:{a:2}}]"))
          (Left (SolverErrors (RowMismatch (TypeRow {location = ArrayElementCursor 0 ExpressionCursor, typeVariable = Nothing, fields = [Field {location = ArrayElementCursor 0 (RecordFieldCursor (FieldName {unFieldName = "x"}) TypeCursor), name = FieldName {unFieldName = "x"}, typ = VariableType (TypeVariable {location = ArrayElementCursor 0 (RecordFieldCursor (FieldName {unFieldName = "x"}) (RowFieldExpression ExpressionCursor)), prefix = ApplyPrefix, index = 2, kind = TypeKind})}]}) (TypeRow {location = ArrayElementCursor 1 ExpressionCursor, typeVariable = Nothing, fields = [Field {location = ArrayElementCursor 1 (RecordFieldCursor (FieldName {unFieldName = "y"}) TypeCursor), name = FieldName {unFieldName = "y"}, typ = RecordType (RowType (TypeRow {location = ArrayElementCursor 1 (RecordFieldCursor (FieldName {unFieldName = "y"}) (RowFieldExpression ExpressionCursor)), typeVariable = Nothing, fields = [Field {location = ArrayElementCursor 1 (RecordFieldCursor (FieldName {unFieldName = "y"}) (RowFieldExpression (RecordFieldCursor (FieldName {unFieldName = "a"}) TypeCursor))), name = FieldName {unFieldName = "a"}, typ = VariableType (TypeVariable {location = ArrayElementCursor 1 (RecordFieldCursor (FieldName {unFieldName = "y"}) (RowFieldExpression (RecordFieldCursor (FieldName {unFieldName = "a"}) (RowFieldExpression ExpressionCursor)))), prefix = ApplyPrefix, index = 4, kind = TypeKind})}]}))}]}) :| [])))))

--------------------------------------------------------------------------------
-- Fine-grained tests

fineGrained :: Spec
fineGrained = do
  describe
    "Successful"
    (do it "a ~ a" (shouldBe (unifyConstraints' [a .~ a]) (pure []))
        it
          "Integer ~ Integer"
          (shouldBe (unifyConstraints' [_Integer .~ _Integer]) (pure []))
        it "a ~ b" (shouldBe (unifyConstraints' [a .~ b]) (pure [a' .+-> b]))
        it
          "a ~ Integer"
          (shouldBe (unifyConstraints' [a .~ _Integer]) (pure [a' .+-> _Integer]))
        it
          "F a b ~ F Text a"
          (shouldBe
             (unifyConstraints' [_F a b .~ _F _Text a])
             (pure [a' .+-> _Text, b' .+-> _Text]))
        it
          "F a Text ~ F Text a"
          (shouldBe
             (unifyConstraints' [_F a _Text .~ _F _Text a])
             (pure [a' .+-> _Text]))
        it
          "F a Text ~ F Integer b"
          (shouldBe
             (unifyConstraints' [_F a _Text .~ _F _Integer b])
             (pure [a' .+-> _Integer, b' .+-> _Text]))
        it
          "F a a ~ F (Option b) (Option Integer)"
          (shouldBe
             (unifyConstraints' [_F a a .~ _F (_Option b) (_Option _Integer)])
             (pure [a' .+-> _Option _Integer, b' .+-> _Integer]))
        it
          "(t ~ F a a, F a a ~ F (Option b) (Option Integer)) => t"
          (shouldBe
             (unifyAndSubstitute'
                [t .~ _F a a, _F a a .~ _F (_Option b) (_Option _Integer)]
                t)
             (pure (solveType mempty (_F (_Option _Integer) (_Option _Integer))))))
  describe
    "Failing"
    (do it
          "Occurs check: F a b ~ a"
          (shouldBe
             (unifyConstraints' [_F a b .~ a])
             (Left (pure (OccursCheckFail a' (_F a b)))))
        it
          "Kind mismatch: F a ~ b"
          (shouldBe
             (unifyConstraints' [_F_partial a .~ b])
             (Left (pure (KindMismatch b' (_F_partial a)))))
        it
          "Constant mismatch: Integer ~ Text"
          (shouldBe
             (unifyConstraints' [_Integer .~ _Text])
             (Left (pure (TypeMismatch (_Integer .~ _Text)))))
        it
          "Type mismatch: F a a ~ F (Option Text) (Option Integer)"
          (shouldBe
             (unifyConstraints' [_F a a .~ _F (_Option _Text) (_Option _Integer)])
             (Left (pure (TypeMismatch (_Text .~ _Integer))))))

--------------------------------------------------------------------------------
-- Type variables

a' :: TypeVariable Generated
a' =
  TypeVariable
    {location = ExpressionCursor, prefix = IntegerPrefix, index = 0, kind = TypeKind}

b' :: TypeVariable Generated
b' =
  TypeVariable
    {location = ExpressionCursor, prefix = IntegerPrefix, index = 1, kind = TypeKind}

c' :: TypeVariable Generated
c' =
  TypeVariable
    {location = ExpressionCursor, prefix = IntegerPrefix, index = 2, kind = TypeKind}

--------------------------------------------------------------------------------
-- Types of the variables

t :: Type Generated
t =
  VariableType
    TypeVariable
      {location = ExpressionCursor, prefix = IntegerPrefix, index = 3, kind = TypeKind}

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

--------------------------------------------------------------------------------
-- Coarse grained

variants :: SpecWith ()
variants = do
  it
    "[#ok(1),#fail]"
    (shouldBe (fmap Inflex.Solver.thing (solveText' mempty "" "[#ok(1),#fail]"))
              (Right (ArrayExpression (Array {expressions = [VariantExpression (Variant {location = ArrayElementCursor 0 ExpressionCursor, typ = VariantType (RowType (TypeRow {location = ArrayElementCursor 0 ExpressionCursor, typeVariable = Just (TypeVariable {location = ArrayElementCursor 0 ExpressionCursor, prefix = SolverGeneratedPrefix RowUnifyPrefix, index = 0, kind = RowKind}), fields = [Field {location = ArrayElementCursor 1 ExpressionCursor, name = FieldName {unFieldName = "fail"}, typ = RowType (TypeRow {location = ArrayElementCursor 1 ExpressionCursor, typeVariable = Nothing, fields = []})},Field {location = ArrayElementCursor 0 ExpressionCursor, name = FieldName {unFieldName = "ok"}, typ = VariableType (TypeVariable {location = ArrayElementCursor 0 (VariantElementCursor ExpressionCursor), prefix = ApplyPrefix, index = 3, kind = TypeKind})}]})), tag = TagName {unTagName = "ok"}, argument = Just (ApplyExpression (Apply {location = BuiltIn, function = GlobalExpression (Global {location = BuiltIn, name = FromIntegerGlobal, scheme = SolvedScheme (Scheme {location = BuiltIn, constraints = [ClassConstraint {className = FromIntegerClassName, typ = VariableType (TypeVariable {location = ArrayElementCursor 0 (VariantElementCursor ExpressionCursor), prefix = ApplyPrefix, index = 3, kind = TypeKind}) :| [], location = BuiltIn}], typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = BuiltIn, name = FunctionTypeName}), argument = ConstantType (TypeConstant {location = BuiltIn, name = IntegerTypeName}), location = BuiltIn, kind = FunKind TypeKind TypeKind}), argument = VariableType (TypeVariable {location = ArrayElementCursor 0 (VariantElementCursor ExpressionCursor), prefix = ApplyPrefix, index = 3, kind = TypeKind}), location = BuiltIn, kind = TypeKind})})}), argument = LiteralExpression (NumberLiteral (Number {location = ArrayElementCursor 0 (VariantElementCursor ExpressionCursor), number = IntegerNumber 1, typ = ConstantType (TypeConstant {location = ArrayElementCursor 0 (VariantElementCursor ExpressionCursor), name = IntegerTypeName})})), typ = VariableType (TypeVariable {location = ArrayElementCursor 0 (VariantElementCursor ExpressionCursor), prefix = ApplyPrefix, index = 3, kind = TypeKind})}))}),VariantExpression (Variant {location = ArrayElementCursor 1 ExpressionCursor, typ = VariantType (RowType (TypeRow {location = ArrayElementCursor 1 ExpressionCursor, typeVariable = Just (TypeVariable {location = ArrayElementCursor 0 ExpressionCursor, prefix = SolverGeneratedPrefix RowUnifyPrefix, index = 0, kind = RowKind}), fields = [Field {location = ArrayElementCursor 1 ExpressionCursor, name = FieldName {unFieldName = "fail"}, typ = RowType (TypeRow {location = ArrayElementCursor 1 ExpressionCursor, typeVariable = Nothing, fields = []})},Field {location = ArrayElementCursor 0 ExpressionCursor, name = FieldName {unFieldName = "ok"}, typ = VariableType (TypeVariable {location = ArrayElementCursor 0 (VariantElementCursor ExpressionCursor), prefix = ApplyPrefix, index = 3, kind = TypeKind})}]})), tag = TagName {unTagName = "fail"}, argument = Nothing})], typ = ArrayType (VariantType (RowType (TypeRow {location = ArrayElementCursor 0 ExpressionCursor, typeVariable = Just (TypeVariable {location = ArrayElementCursor 0 ExpressionCursor, prefix = SolverGeneratedPrefix RowUnifyPrefix, index = 0, kind = RowKind}), fields = [Field {location = ArrayElementCursor 1 ExpressionCursor, name = FieldName {unFieldName = "fail"}, typ = RowType (TypeRow {location = ArrayElementCursor 1 ExpressionCursor, typeVariable = Nothing, fields = []})},Field {location = ArrayElementCursor 0 ExpressionCursor, name = FieldName {unFieldName = "ok"}, typ = VariableType (TypeVariable {location = ArrayElementCursor 0 (VariantElementCursor ExpressionCursor), prefix = ApplyPrefix, index = 3, kind = TypeKind})}]}))), location = ExpressionCursor}))))


arrays :: SpecWith ()
arrays = do
  it
    "[[1],[2]]"
    (shouldBe
       (fmap Inflex.Solver.thing (solveText' mempty "" "[[1],[2]]"))
       (Right (ArrayExpression (Array {expressions = [ArrayExpression (Array {expressions = [ApplyExpression (Apply {location = BuiltIn, function = GlobalExpression (Global {location = BuiltIn, name = FromIntegerGlobal, scheme = SolvedScheme (Scheme {location = BuiltIn, constraints = [ClassConstraint {className = FromIntegerClassName, typ = VariableType (TypeVariable {location = ArrayElementCursor 1 (ArrayElementCursor 0 ExpressionCursor), prefix = ApplyPrefix, index = 6, kind = TypeKind}) :| [], location = BuiltIn}], typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = BuiltIn, name = FunctionTypeName}), argument = ConstantType (TypeConstant {location = BuiltIn, name = IntegerTypeName}), location = BuiltIn, kind = FunKind TypeKind TypeKind}), argument = VariableType (TypeVariable {location = ArrayElementCursor 1 (ArrayElementCursor 0 ExpressionCursor), prefix = ApplyPrefix, index = 6, kind = TypeKind}), location = BuiltIn, kind = TypeKind})})}), argument = LiteralExpression (NumberLiteral (Number {location = ArrayElementCursor 0 (ArrayElementCursor 0 ExpressionCursor), number = IntegerNumber 1, typ = ConstantType (TypeConstant {location = ArrayElementCursor 0 (ArrayElementCursor 0 ExpressionCursor), name = IntegerTypeName})})), typ = VariableType (TypeVariable {location = ArrayElementCursor 1 (ArrayElementCursor 0 ExpressionCursor), prefix = ApplyPrefix, index = 6, kind = TypeKind})})], typ = ArrayType (VariableType (TypeVariable {location = ArrayElementCursor 1 (ArrayElementCursor 0 ExpressionCursor), prefix = ApplyPrefix, index = 6, kind = TypeKind})), location = ArrayElementCursor 0 ExpressionCursor}),ArrayExpression (Array {expressions = [ApplyExpression (Apply {location = BuiltIn, function = GlobalExpression (Global {location = BuiltIn, name = FromIntegerGlobal, scheme = SolvedScheme (Scheme {location = BuiltIn, constraints = [ClassConstraint {className = FromIntegerClassName, typ = VariableType (TypeVariable {location = ArrayElementCursor 1 (ArrayElementCursor 0 ExpressionCursor), prefix = ApplyPrefix, index = 6, kind = TypeKind}) :| [], location = BuiltIn}], typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = BuiltIn, name = FunctionTypeName}), argument = ConstantType (TypeConstant {location = BuiltIn, name = IntegerTypeName}), location = BuiltIn, kind = FunKind TypeKind TypeKind}), argument = VariableType (TypeVariable {location = ArrayElementCursor 1 (ArrayElementCursor 0 ExpressionCursor), prefix = ApplyPrefix, index = 6, kind = TypeKind}), location = BuiltIn, kind = TypeKind})})}), argument = LiteralExpression (NumberLiteral (Number {location = ArrayElementCursor 1 (ArrayElementCursor 0 ExpressionCursor), number = IntegerNumber 2, typ = ConstantType (TypeConstant {location = ArrayElementCursor 1 (ArrayElementCursor 0 ExpressionCursor), name = IntegerTypeName})})), typ = VariableType (TypeVariable {location = ArrayElementCursor 1 (ArrayElementCursor 0 ExpressionCursor), prefix = ApplyPrefix, index = 6, kind = TypeKind})})], typ = ArrayType (VariableType (TypeVariable {location = ArrayElementCursor 1 (ArrayElementCursor 0 ExpressionCursor), prefix = ApplyPrefix, index = 6, kind = TypeKind})), location = ArrayElementCursor 1 ExpressionCursor})], typ = ArrayType (ArrayType (VariableType (TypeVariable {location = ArrayElementCursor 1 (ArrayElementCursor 0 ExpressionCursor), prefix = ApplyPrefix, index = 6, kind = TypeKind}))), location = ExpressionCursor}))))
  arrayHoles

arrayHoles :: Spec
arrayHoles = do
  it
    "[] :: [_]"
    (shouldBe
       (fmap Inflex.Solver.thing (solveText' mempty "" "[] :: [_]"))
       (Right
          (ArrayExpression
             (Array
                { expressions = []
                , typ =
                    ArrayType
                      (VariableType
                         (TypeVariable
                            { location = ExpressionCursor
                            , prefix = ArrayElementPrefix
                            , index = 0
                            , kind = TypeKind
                            }))
                , location = ExpressionCursor
                }))))
  it
    "[] :: [{a:_}]"
    (shouldBe
       (fmap Inflex.Solver.thing (solveText' mempty "" "[] :: [{a:_}]"))
       (Right
          (ArrayExpression
             (Array
                { expressions = []
                , typ =
                    ArrayType
                      (RecordType
                         (RowType
                            (TypeRow
                               { location = SignatureCursor TypeCursor
                               , typeVariable = Nothing
                               , fields =
                                   [ Field
                                       { location =
                                           SignatureCursor
                                             (RowFieldCursor TypeCursor)
                                       , name = FieldName {unFieldName = "a"}
                                       , typ =
                                           VariableType
                                             (TypeVariable
                                                { location =
                                                    SignatureCursor
                                                      (RowFieldCursor
                                                         (RowFieldType
                                                            LambdaParamCursor))
                                                , prefix = FreshPrefix
                                                , index = 1
                                                , kind = TypeKind
                                                })
                                       }
                                   ]
                               })))
                , location = ExpressionCursor
                }))))
  it
    "[] :: [{a,b}]"
    (shouldBe
       (fmap Inflex.Solver.thing (solveText' mempty "" "[] :: [{a,b}]"))
       (Right
          (ArrayExpression
             (Array
                { expressions = []
                , typ =
                    ArrayType
                      (RecordType
                         (RowType
                            (TypeRow
                               { location = SignatureCursor TypeCursor
                               , typeVariable = Nothing
                               , fields =
                                   [ Field
                                       { location =
                                           SignatureCursor
                                             (RowFieldCursor TypeCursor)
                                       , name = FieldName {unFieldName = "a"}
                                       , typ =
                                           VariableType
                                             (TypeVariable
                                                { location =
                                                    SignatureCursor
                                                      (RowFieldCursor
                                                         (RowFieldType
                                                            LambdaParamCursor))
                                                , prefix = FreshPrefix
                                                , index = 1
                                                , kind = TypeKind
                                                })
                                       }
                                   , Field
                                       { location =
                                           SignatureCursor
                                             (RowFieldCursor TypeCursor)
                                       , name = FieldName {unFieldName = "b"}
                                       , typ =
                                           VariableType
                                             (TypeVariable
                                                { location =
                                                    SignatureCursor
                                                      (RowFieldCursor
                                                         (RowFieldType
                                                            LambdaParamCursor))
                                                , prefix = FreshPrefix
                                                , index = 2
                                                , kind = TypeKind
                                                })
                                       }
                                   ]
                               })))
                , location = ExpressionCursor
                }))))
  it
    "[] :: [{a,b:{x}]"
    (shouldBe
       (fmap Inflex.Solver.thing (solveText' mempty "" "[] :: [{a,b:{x}}]"))
       (Right
          (ArrayExpression
             (Array
                { expressions = []
                , typ =
                    ArrayType
                      (RecordType
                         (RowType
                            (TypeRow
                               { location = SignatureCursor TypeCursor
                               , typeVariable = Nothing
                               , fields =
                                   [ Field
                                       { location =
                                           SignatureCursor
                                             (RowFieldCursor TypeCursor)
                                       , name = FieldName {unFieldName = "a"}
                                       , typ =
                                           VariableType
                                             (TypeVariable
                                                { location =
                                                    SignatureCursor
                                                      (RowFieldCursor
                                                         (RowFieldType
                                                            LambdaParamCursor))
                                                , prefix = FreshPrefix
                                                , index = 1
                                                , kind = TypeKind
                                                })
                                       }
                                   , Field
                                       { location =
                                           SignatureCursor
                                             (RowFieldCursor TypeCursor)
                                       , name = FieldName {unFieldName = "b"}
                                       , typ =
                                           RecordType
                                             (RowType
                                                (TypeRow
                                                   { location =
                                                       SignatureCursor
                                                         (RowFieldCursor
                                                            (RowFieldType
                                                               TypeCursor))
                                                   , typeVariable = Nothing
                                                   , fields =
                                                       [ Field
                                                           { location =
                                                               SignatureCursor
                                                                 (RowFieldCursor
                                                                    (RowFieldType
                                                                       (RowFieldCursor
                                                                          TypeCursor)))
                                                           , name =
                                                               FieldName
                                                                 { unFieldName =
                                                                     "x"
                                                                 }
                                                           , typ =
                                                               VariableType
                                                                 (TypeVariable
                                                                    { location =
                                                                        SignatureCursor
                                                                          (RowFieldCursor
                                                                             (RowFieldType
                                                                                (RowFieldCursor
                                                                                   (RowFieldType
                                                                                      LambdaParamCursor))))
                                                                    , prefix =
                                                                        FreshPrefix
                                                                    , index = 2
                                                                    , kind =
                                                                        TypeKind
                                                                    })
                                                           }
                                                       ]
                                                   }))
                                       }
                                   ]
                               })))
                , location = ExpressionCursor
                }))))
