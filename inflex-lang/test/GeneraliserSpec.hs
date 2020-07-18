{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- |

module GeneraliserSpec where

import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as M
import           Inflex.Generaliser
import           Inflex.Instances ()
import           Inflex.Types
import           Test.Hspec

spec :: Spec
spec = do
  describe "Fine-grained" fineGrained
  describe "Coarse-grained" coarseGrained

coarseGrained :: Spec
coarseGrained = do
  it
    "fromInteger 123"
    (shouldBe
       (generaliseText mempty "" "fromInteger (123 :: Integer)")
       (Right (IsGeneralised {thing = ApplyExpression (Apply {location = ExpressionCursor, function = GlobalExpression (Global {location = ApplyFuncCursor ExpressionCursor, name = FromIntegerGlobal, scheme = GeneralisedScheme (Scheme {location = ApplyFuncCursor ExpressionCursor, constraints = [ClassConstraint {className = FromIntegerClassName, typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}) :| [], location = ApplyFuncCursor ExpressionCursor}], typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = ApplyFuncCursor ExpressionCursor, name = FunctionTypeName}), argument = ConstantType (TypeConstant {location = ApplyFuncCursor ExpressionCursor, name = IntegerTypeName}), location = ApplyFuncCursor ExpressionCursor, kind = FunKind TypeKind TypeKind}), argument = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = ApplyFuncCursor ExpressionCursor, kind = TypeKind})})}), argument = LiteralExpression (NumberLiteral (Number {location = ApplyArgCursor ExpressionCursor, number = IntegerNumber 123, typ = ConstantType (TypeConstant {location = ApplyArgCursor ExpressionCursor, name = IntegerTypeName})})), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}), polytype = VariableType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), mappings = M.fromList [(ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 1, name = ""}, end = SourcePos {line = 1, column = 17, name = ""}}),(ApplyFuncCursor ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 1, name = ""}, end = SourcePos {line = 1, column = 12, name = ""}}),(ApplyArgCursor ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 14, name = ""}, end = SourcePos {line = 1, column = 17, name = ""}}),(ApplyArgCursor (SignatureCursor TypeCursor),SourceLocation {start = SourcePos {line = 1, column = 21, name = ""}, end = SourcePos {line = 1, column = 28, name = ""}})]})))
  it
    "\\x->123"
    (shouldBe
       (generaliseText mempty "" "\\x->(123::Integer)")
       (Right (IsGeneralised {thing = LambdaExpression (Lambda {location = ExpressionCursor, param = Param {location = LambdaParamCursor, name = (), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}, body = LiteralExpression (NumberLiteral (Number {location = LambdaBodyCursor ExpressionCursor, number = IntegerNumber 123, typ = ConstantType (TypeConstant {location = LambdaBodyCursor ExpressionCursor, name = IntegerTypeName})})), typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = ExpressionCursor, name = FunctionTypeName}), argument = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = ExpressionCursor, kind = FunKind TypeKind TypeKind}), argument = ConstantType (TypeConstant {location = LambdaBodyCursor ExpressionCursor, name = IntegerTypeName}), location = ExpressionCursor, kind = TypeKind})}), polytype = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = ExpressionCursor, name = FunctionTypeName}), argument = VariableType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = ExpressionCursor, kind = FunKind TypeKind TypeKind}), argument = ConstantType (TypeConstant {location = LambdaBodyCursor ExpressionCursor, name = IntegerTypeName}), location = ExpressionCursor, kind = TypeKind}), mappings = M.fromList [(ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 1, name = ""}, end = SourcePos {line = 1, column = 9, name = ""}}),(LambdaBodyCursor ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 6, name = ""}, end = SourcePos {line = 1, column = 9, name = ""}}),(LambdaBodyCursor (SignatureCursor TypeCursor),SourceLocation {start = SourcePos {line = 1, column = 11, name = ""}, end = SourcePos {line = 1, column = 18, name = ""}}),(LambdaParamCursor,SourceLocation {start = SourcePos {line = 1, column = 2, name = ""}, end = SourcePos {line = 1, column = 3, name = ""}})]})))
  it
    "(\\x->123)(\\x->x)"
    (shouldBe
       (generaliseText mempty "" "(\\x->(123::Integer))(\\x->x)")
       (Right (IsGeneralised {thing = ApplyExpression (Apply {location = ExpressionCursor, function = LambdaExpression (Lambda {location = ApplyFuncCursor ExpressionCursor, param = Param {location = ApplyFuncCursor LambdaParamCursor, name = (), typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = ApplyArgCursor ExpressionCursor, name = FunctionTypeName}), argument = VariableType (TypeVariable {location = ApplyArgCursor LambdaParamCursor, prefix = LambdaParameterPrefix, index = 1, kind = TypeKind}), location = ApplyArgCursor ExpressionCursor, kind = FunKind TypeKind TypeKind}), argument = VariableType (TypeVariable {location = ApplyArgCursor LambdaParamCursor, prefix = LambdaParameterPrefix, index = 1, kind = TypeKind}), location = ApplyArgCursor ExpressionCursor, kind = TypeKind})}, body = LiteralExpression (NumberLiteral (Number {location = ApplyFuncCursor (LambdaBodyCursor ExpressionCursor), number = IntegerNumber 123, typ = ConstantType (TypeConstant {location = ApplyFuncCursor (LambdaBodyCursor ExpressionCursor), name = IntegerTypeName})})), typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = ApplyFuncCursor ExpressionCursor, name = FunctionTypeName}), argument = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = ApplyArgCursor ExpressionCursor, name = FunctionTypeName}), argument = VariableType (TypeVariable {location = ApplyArgCursor LambdaParamCursor, prefix = LambdaParameterPrefix, index = 1, kind = TypeKind}), location = ApplyArgCursor ExpressionCursor, kind = FunKind TypeKind TypeKind}), argument = VariableType (TypeVariable {location = ApplyArgCursor LambdaParamCursor, prefix = LambdaParameterPrefix, index = 1, kind = TypeKind}), location = ApplyArgCursor ExpressionCursor, kind = TypeKind}), location = ApplyFuncCursor ExpressionCursor, kind = FunKind TypeKind TypeKind}), argument = ConstantType (TypeConstant {location = ApplyFuncCursor (LambdaBodyCursor ExpressionCursor), name = IntegerTypeName}), location = ApplyFuncCursor ExpressionCursor, kind = TypeKind})}), argument = LambdaExpression (Lambda {location = ApplyArgCursor ExpressionCursor, param = Param {location = ApplyArgCursor LambdaParamCursor, name = (), typ = VariableType (TypeVariable {location = ApplyArgCursor LambdaParamCursor, prefix = LambdaParameterPrefix, index = 1, kind = TypeKind})}, body = VariableExpression (Variable {location = ApplyArgCursor (LambdaBodyCursor ExpressionCursor), name = DeBrujinIndex (DeBrujinNesting 0), typ = VariableType (TypeVariable {location = ApplyArgCursor LambdaParamCursor, prefix = LambdaParameterPrefix, index = 1, kind = TypeKind})}), typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = ApplyArgCursor ExpressionCursor, name = FunctionTypeName}), argument = VariableType (TypeVariable {location = ApplyArgCursor LambdaParamCursor, prefix = LambdaParameterPrefix, index = 1, kind = TypeKind}), location = ApplyArgCursor ExpressionCursor, kind = FunKind TypeKind TypeKind}), argument = VariableType (TypeVariable {location = ApplyArgCursor LambdaParamCursor, prefix = LambdaParameterPrefix, index = 1, kind = TypeKind}), location = ApplyArgCursor ExpressionCursor, kind = TypeKind})}), typ = ConstantType (TypeConstant {location = ApplyFuncCursor (LambdaBodyCursor ExpressionCursor), name = IntegerTypeName})}), polytype = ConstantType (TypeConstant {location = ApplyFuncCursor (LambdaBodyCursor ExpressionCursor), name = IntegerTypeName}), mappings = M.fromList [(ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 2, name = ""}, end = SourcePos {line = 1, column = 27, name = ""}}),(ApplyFuncCursor ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 2, name = ""}, end = SourcePos {line = 1, column = 10, name = ""}}),(ApplyFuncCursor (LambdaBodyCursor ExpressionCursor),SourceLocation {start = SourcePos {line = 1, column = 7, name = ""}, end = SourcePos {line = 1, column = 10, name = ""}}),(ApplyFuncCursor (LambdaBodyCursor (SignatureCursor TypeCursor)),SourceLocation {start = SourcePos {line = 1, column = 12, name = ""}, end = SourcePos {line = 1, column = 19, name = ""}}),(ApplyFuncCursor LambdaParamCursor,SourceLocation {start = SourcePos {line = 1, column = 3, name = ""}, end = SourcePos {line = 1, column = 4, name = ""}}),(ApplyArgCursor ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 22, name = ""}, end = SourcePos {line = 1, column = 27, name = ""}}),(ApplyArgCursor (LambdaBodyCursor ExpressionCursor),SourceLocation {start = SourcePos {line = 1, column = 26, name = ""}, end = SourcePos {line = 1, column = 27, name = ""}}),(ApplyArgCursor LambdaParamCursor,SourceLocation {start = SourcePos {line = 1, column = 23, name = ""}, end = SourcePos {line = 1, column = 24, name = ""}})]})))

fineGrained :: Spec
fineGrained =
  it
    "Polymorphise a type"
    (shouldBe
       (toPolymorphic
          (ApplyType
             (TypeApplication
                { function =
                    ApplyType
                      (TypeApplication
                         { function =
                             ConstantType
                               (TypeConstant
                                  { location = ApplyFuncCursor ExpressionCursor
                                  , name = FunctionTypeName
                                  })
                         , argument =
                             VariableType
                               (TypeVariable
                                  { location = ApplyArgCursor ExpressionCursor
                                  , prefix = ApplyPrefix
                                  , index = 2
                                  , kind = TypeKind
                                  })
                         , location = ApplyFuncCursor ExpressionCursor
                         , kind = FunKind TypeKind TypeKind
                         })
                , argument =
                    VariableType
                      (TypeVariable
                         { location = ApplyArgCursor ExpressionCursor
                         , prefix = ApplyPrefix
                         , index = 3
                         , kind = TypeKind
                         })
                , location = ApplyFuncCursor ExpressionCursor
                , kind = TypeKind
                })))
       ( ApplyType
           (TypeApplication
              { function =
                  ApplyType
                    (TypeApplication
                       { function =
                           ConstantType
                             (TypeConstant
                                { location = ApplyFuncCursor ExpressionCursor
                                , name = FunctionTypeName
                                })
                       , argument =
                           VariableType
                             (TypeVariable
                                { location = ()
                                , prefix = ()
                                , index = 0
                                , kind = TypeKind
                                })
                       , location = ApplyFuncCursor ExpressionCursor
                       , kind = FunKind TypeKind TypeKind
                       })
              , argument =
                  VariableType
                    (TypeVariable
                       {location = (), prefix = (), index = 1, kind = TypeKind})
              , location = ApplyFuncCursor ExpressionCursor
              , kind = TypeKind
              })
       , M.fromList
           [ ( TypeVariable
                 { location = ApplyArgCursor ExpressionCursor
                 , prefix = ApplyPrefix
                 , index = 2
                 , kind = TypeKind
                 }
             , TypeVariable
                 {location = (), prefix = (), index = 0, kind = TypeKind})
           , ( TypeVariable
                 { location = ApplyArgCursor ExpressionCursor
                 , prefix = ApplyPrefix
                 , index = 3
                 , kind = TypeKind
                 }
             , TypeVariable
                 {location = (), prefix = (), index = 1, kind = TypeKind})
           ]))
