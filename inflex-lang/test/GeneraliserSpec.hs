{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- |

module GeneraliserSpec where

import qualified Data.Map.Strict as M
import           Inflex.Generaliser
import           Inflex.Instances ()
import           Inflex.Types
import           Test.Hspec

spec :: Spec
spec = describe "Fine-grained" fineGrained

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
