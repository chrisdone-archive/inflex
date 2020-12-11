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
import           Data.Text (Text)
import           Inflex.Generaliser
import           Inflex.Instances ()
import           Inflex.Types
import           Test.Syd

generaliseText' :: (e~ ())=>
     M.Map Hash (Either e (Scheme Polymorphic))
  -> FilePath
  -> Text
  -> Either (SolveGeneraliseError e) (IsGeneralised (Expression Generalised))
generaliseText' = generaliseText

spec :: Spec
spec = do
  describe "Fine-grained" fineGrained
  describe "Coarse-grained" coarseGrained

coarseGrained :: Spec
coarseGrained = do
  records
  it
    "fromInteger 123"
    (shouldBe
       (generaliseText' mempty "" "fromInteger (123 :: Integer)")
       (Right (IsGeneralised {thing = ApplyExpression (Apply {location = ExpressionCursor, function = GlobalExpression (Global {location = ApplyFuncCursor ExpressionCursor, name = FromIntegerGlobal, scheme = GeneralisedScheme (Scheme {location = ApplyFuncCursor ExpressionCursor, constraints = [ClassConstraint {className = FromIntegerClassName, typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}) :| [], location = ApplyFuncCursor ExpressionCursor}], typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = BuiltIn, name = FunctionTypeName}), argument = ConstantType (TypeConstant {location = BuiltIn, name = IntegerTypeName}), location = BuiltIn, kind = FunKind TypeKind TypeKind}), argument = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = BuiltIn, kind = TypeKind})})}), argument = LiteralExpression (NumberLiteral (Number {location = ApplyArgCursor ExpressionCursor, number = IntegerNumber 123, typ = ConstantType (TypeConstant {location = ApplyArgCursor ExpressionCursor, name = IntegerTypeName})})), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}), polytype = VariableType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), mappings = M.fromList [(ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 14, name = ""}, end = SourcePos {line = 1, column = 17, name = ""}}),(ApplyFuncCursor ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 1, name = ""}, end = SourcePos {line = 1, column = 12, name = ""}}),(ApplyArgCursor ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 14, name = ""}, end = SourcePos {line = 1, column = 17, name = ""}}),(ApplyArgCursor (SignatureCursor TypeCursor),SourceLocation {start = SourcePos {line = 1, column = 21, name = ""}, end = SourcePos {line = 1, column = 28, name = ""}})]})))
  it
    "x:123"
    (shouldBe
       (generaliseText' mempty "" "x:(123::Integer)")
       (Right (IsGeneralised {thing = LambdaExpression (Lambda {location = ExpressionCursor, param = Param {location = LambdaParamCursor, name = (), typ = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind})}, body = LiteralExpression (NumberLiteral (Number {location = LambdaBodyCursor ExpressionCursor, number = IntegerNumber 123, typ = ConstantType (TypeConstant {location = LambdaBodyCursor ExpressionCursor, name = IntegerTypeName})})), typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = ExpressionCursor, name = FunctionTypeName}), argument = PolyType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = ExpressionCursor, kind = FunKind TypeKind TypeKind}), argument = ConstantType (TypeConstant {location = LambdaBodyCursor ExpressionCursor, name = IntegerTypeName}), location = ExpressionCursor, kind = TypeKind})}), polytype = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = ExpressionCursor, name = FunctionTypeName}), argument = VariableType (TypeVariable {location = (), prefix = (), index = 0, kind = TypeKind}), location = ExpressionCursor, kind = FunKind TypeKind TypeKind}), argument = ConstantType (TypeConstant {location = LambdaBodyCursor ExpressionCursor, name = IntegerTypeName}), location = ExpressionCursor, kind = TypeKind}), mappings = M.fromList [(ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 1, name = ""}, end = SourcePos {line = 1, column = 7, name = ""}}),(LambdaBodyCursor ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 4, name = ""}, end = SourcePos {line = 1, column = 7, name = ""}}),(LambdaBodyCursor (SignatureCursor TypeCursor),SourceLocation {start = SourcePos {line = 1, column = 9, name = ""}, end = SourcePos {line = 1, column = 16, name = ""}}),(LambdaParamCursor,SourceLocation {start = SourcePos {line = 1, column = 1, name = ""}, end = SourcePos {line = 1, column = 2, name = ""}})]})))
  it
    "(x:123)(x:x)"
    (shouldBe
       (generaliseText' mempty "" "(x:(123::Integer))(x:x)")
       (Right (IsGeneralised {thing = ApplyExpression (Apply {location = ExpressionCursor, function = LambdaExpression (Lambda {location = ApplyFuncCursor ExpressionCursor, param = Param {location = ApplyFuncCursor LambdaParamCursor, name = (), typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = ApplyArgCursor ExpressionCursor, name = FunctionTypeName}), argument = VariableType (TypeVariable {location = ApplyArgCursor LambdaParamCursor, prefix = LambdaParameterPrefix, index = 1, kind = TypeKind}), location = ApplyArgCursor ExpressionCursor, kind = FunKind TypeKind TypeKind}), argument = VariableType (TypeVariable {location = ApplyArgCursor LambdaParamCursor, prefix = LambdaParameterPrefix, index = 1, kind = TypeKind}), location = ApplyArgCursor ExpressionCursor, kind = TypeKind})}, body = LiteralExpression (NumberLiteral (Number {location = ApplyFuncCursor (LambdaBodyCursor ExpressionCursor), number = IntegerNumber 123, typ = ConstantType (TypeConstant {location = ApplyFuncCursor (LambdaBodyCursor ExpressionCursor), name = IntegerTypeName})})), typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = ApplyFuncCursor ExpressionCursor, name = FunctionTypeName}), argument = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = ApplyArgCursor ExpressionCursor, name = FunctionTypeName}), argument = VariableType (TypeVariable {location = ApplyArgCursor LambdaParamCursor, prefix = LambdaParameterPrefix, index = 1, kind = TypeKind}), location = ApplyArgCursor ExpressionCursor, kind = FunKind TypeKind TypeKind}), argument = VariableType (TypeVariable {location = ApplyArgCursor LambdaParamCursor, prefix = LambdaParameterPrefix, index = 1, kind = TypeKind}), location = ApplyArgCursor ExpressionCursor, kind = TypeKind}), location = ApplyFuncCursor ExpressionCursor, kind = FunKind TypeKind TypeKind}), argument = ConstantType (TypeConstant {location = ApplyFuncCursor (LambdaBodyCursor ExpressionCursor), name = IntegerTypeName}), location = ApplyFuncCursor ExpressionCursor, kind = TypeKind})}), argument = LambdaExpression (Lambda {location = ApplyArgCursor ExpressionCursor, param = Param {location = ApplyArgCursor LambdaParamCursor, name = (), typ = VariableType (TypeVariable {location = ApplyArgCursor LambdaParamCursor, prefix = LambdaParameterPrefix, index = 1, kind = TypeKind})}, body = VariableExpression (Variable {location = ApplyArgCursor (LambdaBodyCursor ExpressionCursor), name = DeBrujinIndex (DeBrujinNesting 0), typ = VariableType (TypeVariable {location = ApplyArgCursor LambdaParamCursor, prefix = LambdaParameterPrefix, index = 1, kind = TypeKind})}), typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = ApplyArgCursor ExpressionCursor, name = FunctionTypeName}), argument = VariableType (TypeVariable {location = ApplyArgCursor LambdaParamCursor, prefix = LambdaParameterPrefix, index = 1, kind = TypeKind}), location = ApplyArgCursor ExpressionCursor, kind = FunKind TypeKind TypeKind}), argument = VariableType (TypeVariable {location = ApplyArgCursor LambdaParamCursor, prefix = LambdaParameterPrefix, index = 1, kind = TypeKind}), location = ApplyArgCursor ExpressionCursor, kind = TypeKind})}), typ = ConstantType (TypeConstant {location = ApplyFuncCursor (LambdaBodyCursor ExpressionCursor), name = IntegerTypeName})}), polytype = ConstantType (TypeConstant {location = ApplyFuncCursor (LambdaBodyCursor ExpressionCursor), name = IntegerTypeName}), mappings = M.fromList [(ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 20, name = ""}, end = SourcePos {line = 1, column = 23, name = ""}}),(ApplyFuncCursor ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 2, name = ""}, end = SourcePos {line = 1, column = 8, name = ""}}),(ApplyFuncCursor (LambdaBodyCursor ExpressionCursor),SourceLocation {start = SourcePos {line = 1, column = 5, name = ""}, end = SourcePos {line = 1, column = 8, name = ""}}),(ApplyFuncCursor (LambdaBodyCursor (SignatureCursor TypeCursor)),SourceLocation {start = SourcePos {line = 1, column = 10, name = ""}, end = SourcePos {line = 1, column = 17, name = ""}}),(ApplyFuncCursor LambdaParamCursor,SourceLocation {start = SourcePos {line = 1, column = 2, name = ""}, end = SourcePos {line = 1, column = 3, name = ""}}),(ApplyArgCursor ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 20, name = ""}, end = SourcePos {line = 1, column = 23, name = ""}}),(ApplyArgCursor (LambdaBodyCursor ExpressionCursor),SourceLocation {start = SourcePos {line = 1, column = 22, name = ""}, end = SourcePos {line = 1, column = 23, name = ""}}),(ApplyArgCursor LambdaParamCursor,SourceLocation {start = SourcePos {line = 1, column = 20, name = ""}, end = SourcePos {line = 1, column = 21, name = ""}})]})))

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

records :: SpecWith ()
records = do
  it
    "{a:1}.a"
    (shouldBe
       (fmap
          (\IsGeneralised {thing} -> thing)
          (generaliseText' mempty "" "{a:1}.a"))
       (Right
          (PropExpression
             (Prop
                { expression =
                    RecordExpression
                      (Record
                         { fields =
                             [ FieldE
                                 { name = FieldName {unFieldName = "a"}
                                 , expression =
                                     ApplyExpression
                                       (Apply
                                          { location = BuiltIn
                                          , function =
                                              GlobalExpression
                                                (Global
                                                   { location =
                                                       BuiltIn
                                                   , name = FromIntegerGlobal
                                                   , scheme =
                                                       GeneralisedScheme
                                                         (Scheme
                                                            { location =
                                                                BuiltIn
                                                            , constraints =
                                                                [ ClassConstraint
                                                                    { className =
                                                                        FromIntegerClassName
                                                                    , typ =
                                                                        PolyType
                                                                          (TypeVariable
                                                                             { location =
                                                                                 ()
                                                                             , prefix =
                                                                                 ()
                                                                             , index =
                                                                                 0
                                                                             , kind =
                                                                                 TypeKind
                                                                             }) :|
                                                                        []
                                                                    , location =
                                                                        BuiltIn
                                                                    }
                                                                ]
                                                            , typ =
                                                                ApplyType
                                                                  (TypeApplication
                                                                     { function =
                                                                         ApplyType
                                                                           (TypeApplication
                                                                              { function =
                                                                                  ConstantType
                                                                                    (TypeConstant
                                                                                       { location =
                                                                                           BuiltIn
                                                                                       , name =
                                                                                           FunctionTypeName
                                                                                       })
                                                                              , argument =
                                                                                  ConstantType
                                                                                    (TypeConstant
                                                                                       { location =
                                                                                           BuiltIn
                                                                                       , name =
                                                                                           IntegerTypeName
                                                                                       })
                                                                              , location =
                                                                                  BuiltIn
                                                                              , kind =
                                                                                  FunKind
                                                                                    TypeKind
                                                                                    TypeKind
                                                                              })
                                                                     , argument =
                                                                         PolyType
                                                                           (TypeVariable
                                                                              { location =
                                                                                  ()
                                                                              , prefix =
                                                                                  ()
                                                                              , index =
                                                                                  0
                                                                              , kind =
                                                                                  TypeKind
                                                                              })
                                                                     , location =
                                                                         BuiltIn
                                                                     , kind =
                                                                         TypeKind
                                                                     })
                                                            })
                                                   })
                                          , argument =
                                              LiteralExpression
                                                (NumberLiteral
                                                   (Number
                                                      { location =
                                                          PropExpressionCursor
                                                            (RecordFieldCursor
                                                               (FieldName
                                                                  { unFieldName =
                                                                      "a"
                                                                  })
                                                               (RowFieldExpression
                                                                  ExpressionCursor))
                                                      , number = IntegerNumber 1
                                                      , typ =
                                                          ConstantType
                                                            (TypeConstant
                                                               { location =
                                                                   PropExpressionCursor
                                                                     (RecordFieldCursor
                                                                        (FieldName
                                                                           { unFieldName =
                                                                               "a"
                                                                           })
                                                                        (RowFieldExpression
                                                                           ExpressionCursor))
                                                               , name =
                                                                   IntegerTypeName
                                                               })
                                                      }))
                                          , typ =
                                              PolyType
                                                (TypeVariable
                                                   { location = ()
                                                   , prefix = ()
                                                   , index = 0
                                                   , kind = TypeKind
                                                   })
                                          })
                                 , location =
                                     PropExpressionCursor
                                       (RecordFieldCursor
                                          (FieldName {unFieldName = "a"})
                                          TypeCursor)
                                 }
                             ]
                         , location = PropExpressionCursor ExpressionCursor
                         , typ =
                             RecordType
                               (RowType
                                  (TypeRow
                                     { location =
                                         PropExpressionCursor ExpressionCursor
                                     , typeVariable = Nothing
                                     , fields =
                                         [ Field
                                             { location =
                                                 PropExpressionCursor
                                                   (RecordFieldCursor
                                                      (FieldName
                                                         {unFieldName = "a"})
                                                      TypeCursor)
                                             , name =
                                                 FieldName {unFieldName = "a"}
                                             , typ =
                                                 PolyType
                                                   (TypeVariable
                                                      { location = ()
                                                      , prefix = ()
                                                      , index = 0
                                                      , kind = TypeKind
                                                      })
                                             }
                                         ]
                                     }))
                         })
                , name = FieldName {unFieldName = "a"}
                , typ =
                    PolyType
                      (TypeVariable
                         { location = ()
                         , prefix = ()
                         , index = 0
                         , kind = TypeKind
                         })
                , location = ExpressionCursor
                }))))
  it
    "{a:1, b: 1}.a"
    (shouldBe
       (fmap
          (\IsGeneralised {thing} -> thing)
          (generaliseText' mempty "" "{a:1, b: 1}.a"))
       (Right
          (PropExpression
             (Prop
                { expression =
                    RecordExpression
                      (Record
                         { fields =
                             [ FieldE
                                 { name = FieldName {unFieldName = "a"}
                                 , expression =
                                     ApplyExpression
                                       (Apply
                                          { location = BuiltIn
                                          , function =
                                              GlobalExpression
                                                (Global
                                                   { location =
                                                       BuiltIn
                                                   , name = FromIntegerGlobal
                                                   , scheme =
                                                       GeneralisedScheme
                                                         (Scheme
                                                            { location =
                                                                BuiltIn
                                                            , constraints =
                                                                [ ClassConstraint
                                                                    { className =
                                                                        FromIntegerClassName
                                                                    , typ =
                                                                        PolyType
                                                                          (TypeVariable
                                                                             { location =
                                                                                 ()
                                                                             , prefix =
                                                                                 ()
                                                                             , index =
                                                                                 0
                                                                             , kind =
                                                                                 TypeKind
                                                                             }) :|
                                                                        []
                                                                    , location =
                                                                        BuiltIn
                                                                    }
                                                                ]
                                                            , typ =
                                                                ApplyType
                                                                  (TypeApplication
                                                                     { function =
                                                                         ApplyType
                                                                           (TypeApplication
                                                                              { function =
                                                                                  ConstantType
                                                                                    (TypeConstant
                                                                                       { location =
                                                                                           BuiltIn
                                                                                       , name =
                                                                                           FunctionTypeName
                                                                                       })
                                                                              , argument =
                                                                                  ConstantType
                                                                                    (TypeConstant
                                                                                       { location =
                                                                                           BuiltIn
                                                                                       , name =
                                                                                           IntegerTypeName
                                                                                       })
                                                                              , location =
                                                                                  BuiltIn
                                                                              , kind =
                                                                                  FunKind
                                                                                    TypeKind
                                                                                    TypeKind
                                                                              })
                                                                     , argument =
                                                                         PolyType
                                                                           (TypeVariable
                                                                              { location =
                                                                                  ()
                                                                              , prefix =
                                                                                  ()
                                                                              , index =
                                                                                  0
                                                                              , kind =
                                                                                  TypeKind
                                                                              })
                                                                     , location =
                                                                         BuiltIn
                                                                     , kind =
                                                                         TypeKind
                                                                     })
                                                            })
                                                   })
                                          , argument =
                                              LiteralExpression
                                                (NumberLiteral
                                                   (Number
                                                      { location =
                                                          PropExpressionCursor
                                                            (RecordFieldCursor
                                                               (FieldName
                                                                  { unFieldName =
                                                                      "a"
                                                                  })
                                                               (RowFieldExpression
                                                                  ExpressionCursor))
                                                      , number = IntegerNumber 1
                                                      , typ =
                                                          ConstantType
                                                            (TypeConstant
                                                               { location =
                                                                   PropExpressionCursor
                                                                     (RecordFieldCursor
                                                                        (FieldName
                                                                           { unFieldName =
                                                                               "a"
                                                                           })
                                                                        (RowFieldExpression
                                                                           ExpressionCursor))
                                                               , name =
                                                                   IntegerTypeName
                                                               })
                                                      }))
                                          , typ =
                                              PolyType
                                                (TypeVariable
                                                   { location = ()
                                                   , prefix = ()
                                                   , index = 0
                                                   , kind = TypeKind
                                                   })
                                          })
                                 , location =
                                     PropExpressionCursor
                                       (RecordFieldCursor
                                          (FieldName {unFieldName = "a"})
                                          TypeCursor)
                                 }
                             , FieldE
                                 { name = FieldName {unFieldName = "b"}
                                 , expression =
                                     ApplyExpression
                                       (Apply
                                          { location = BuiltIn
                                          , function =
                                              GlobalExpression
                                                (Global
                                                   { location =
                                                       BuiltIn
                                                   , name = FromIntegerGlobal
                                                   , scheme =
                                                       GeneralisedScheme
                                                         (Scheme
                                                            { location =
                                                                BuiltIn
                                                            , constraints =
                                                                [ ClassConstraint
                                                                    { className =
                                                                        FromIntegerClassName
                                                                    , typ =
                                                                        VariableType
                                                                          (TypeVariable
                                                                             { location =
                                                                                 PropExpressionCursor
                                                                                   (RecordFieldCursor
                                                                                      (FieldName
                                                                                         { unFieldName =
                                                                                             "b"
                                                                                         })
                                                                                      (RowFieldExpression
                                                                                         ExpressionCursor))
                                                                             , prefix =
                                                                                 ApplyPrefix
                                                                             , index =
                                                                                 4
                                                                             , kind =
                                                                                 TypeKind
                                                                             }) :|
                                                                        []
                                                                    , location =
                                                                        BuiltIn
                                                                    }
                                                                ]
                                                            , typ =
                                                                ApplyType
                                                                  (TypeApplication
                                                                     { function =
                                                                         ApplyType
                                                                           (TypeApplication
                                                                              { function =
                                                                                  ConstantType
                                                                                    (TypeConstant
                                                                                       { location =
                                                                                           BuiltIn
                                                                                       , name =
                                                                                           FunctionTypeName
                                                                                       })
                                                                              , argument =
                                                                                  ConstantType
                                                                                    (TypeConstant
                                                                                       { location =
                                                                                           BuiltIn
                                                                                       , name =
                                                                                           IntegerTypeName
                                                                                       })
                                                                              , location =
                                                                                  BuiltIn
                                                                              , kind =
                                                                                  FunKind
                                                                                    TypeKind
                                                                                    TypeKind
                                                                              })
                                                                     , argument =
                                                                         VariableType
                                                                           (TypeVariable
                                                                              { location =
                                                                                  PropExpressionCursor
                                                                                    (RecordFieldCursor
                                                                                       (FieldName
                                                                                          { unFieldName =
                                                                                              "b"
                                                                                          })
                                                                                       (RowFieldExpression
                                                                                          ExpressionCursor))
                                                                              , prefix =
                                                                                  ApplyPrefix
                                                                              , index =
                                                                                  4
                                                                              , kind =
                                                                                  TypeKind
                                                                              })
                                                                     , location =
                                                                         BuiltIn
                                                                     , kind =
                                                                         TypeKind
                                                                     })
                                                            })
                                                   })
                                          , argument =
                                              LiteralExpression
                                                (NumberLiteral
                                                   (Number
                                                      { location =
                                                          PropExpressionCursor
                                                            (RecordFieldCursor
                                                               (FieldName
                                                                  { unFieldName =
                                                                      "b"
                                                                  })
                                                               (RowFieldExpression
                                                                  ExpressionCursor))
                                                      , number = IntegerNumber 1
                                                      , typ =
                                                          ConstantType
                                                            (TypeConstant
                                                               { location =
                                                                   PropExpressionCursor
                                                                     (RecordFieldCursor
                                                                        (FieldName
                                                                           { unFieldName =
                                                                               "b"
                                                                           })
                                                                        (RowFieldExpression
                                                                           ExpressionCursor))
                                                               , name =
                                                                   IntegerTypeName
                                                               })
                                                      }))
                                          , typ =
                                              VariableType
                                                (TypeVariable
                                                   { location =
                                                       PropExpressionCursor
                                                         (RecordFieldCursor
                                                            (FieldName
                                                               { unFieldName =
                                                                   "b"
                                                               })
                                                            (RowFieldExpression
                                                               ExpressionCursor))
                                                   , prefix = ApplyPrefix
                                                   , index = 4
                                                   , kind = TypeKind
                                                   })
                                          })
                                 , location =
                                     PropExpressionCursor
                                       (RecordFieldCursor
                                          (FieldName {unFieldName = "b"})
                                          TypeCursor)
                                 }
                             ]
                         , location = PropExpressionCursor ExpressionCursor
                         , typ =
                             RecordType
                               (RowType
                                  (TypeRow
                                     { location =
                                         PropExpressionCursor ExpressionCursor
                                     , typeVariable = Nothing
                                     , fields =
                                         [ Field
                                             { location =
                                                 PropExpressionCursor
                                                   (RecordFieldCursor
                                                      (FieldName
                                                         {unFieldName = "a"})
                                                      TypeCursor)
                                             , name =
                                                 FieldName {unFieldName = "a"}
                                             , typ =
                                                 PolyType
                                                   (TypeVariable
                                                      { location = ()
                                                      , prefix = ()
                                                      , index = 0
                                                      , kind = TypeKind
                                                      })
                                             }
                                         , Field
                                             { location =
                                                 PropExpressionCursor
                                                   (RecordFieldCursor
                                                      (FieldName
                                                         {unFieldName = "b"})
                                                      TypeCursor)
                                             , name =
                                                 FieldName {unFieldName = "b"}
                                             , typ =
                                                 VariableType
                                                   (TypeVariable
                                                      { location =
                                                          PropExpressionCursor
                                                            (RecordFieldCursor
                                                               (FieldName
                                                                  { unFieldName =
                                                                      "b"
                                                                  })
                                                               (RowFieldExpression
                                                                  ExpressionCursor))
                                                      , prefix = ApplyPrefix
                                                      , index = 4
                                                      , kind = TypeKind
                                                      })
                                             }
                                         ]
                                     }))
                         })
                , name = FieldName {unFieldName = "a"}
                , typ =
                    PolyType
                      (TypeVariable
                         { location = ()
                         , prefix = ()
                         , index = 0
                         , kind = TypeKind
                         })
                , location = ExpressionCursor
                }))))
