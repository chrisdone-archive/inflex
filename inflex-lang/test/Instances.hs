{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Testing instances.

module Instances where

import Data.GenValidity
import GHC.Generics
import Inflex.Instances ()
import Inflex.Types
import Test.QuickCheck

instance GenUnchecked (Type Generated) where
  genUnchecked =
    oneof
      [ VariableType <$> genUnchecked
      , ApplyType <$> genUnchecked
      , ConstantType <$> genUnchecked
      ]
  shrinkUnchecked _ = []

deriving instance Generic (TypeApplication Generated)
instance GenUnchecked (TypeApplication Generated)

instance Validity (Type Generated) where
  validate = mempty

instance GenValid (Type Generated) where
  genValid = genType

genType :: Gen (Type Generated)
genType = do
  size <- getSize
  oneof
    (take
       (if size > 1
          then 3
          else 2)
       [gen0Type, genApp1Type, scale (max 0 . pred) genApp2Type])
  where
    gen0Type =
      oneof
        [ pure
            (ConstantType
               TypeConstant
                 {location = ExpressionCursor, name = IntegerTypeName})
        , pure
            (ConstantType
               TypeConstant {location = ExpressionCursor, name = TextTypeName})
        , do index <- choose (1, 10)
             pure
               (VariableType
                  TypeVariable
                    { location = ExpressionCursor
                    , prefix = IntegeryPrefix
                    , index = fromIntegral (index :: Integer)
                    , kind = TypeKind
                    })
        ]
    gen1Type =
      pure
        (ConstantType
           TypeConstant {location = ExpressionCursor, name = OptionTypeName})
    gen2Type =
      pure
        (ConstantType
           TypeConstant {location = ExpressionCursor, name = FunctionTypeName})
    genApp1Type = do
      func <- gen1Type
      arg <- genType
      pure
        (ApplyType
           TypeApplication
             { function = func
             , argument = arg
             , location = ExpressionCursor
             , kind = TypeKind
             })
    genApp2Type = do
      func <- gen2Type
      arg1 <- genType
      arg2 <- genType
      pure
        (ApplyType
           TypeApplication
             { function =
                 ApplyType
                   TypeApplication
                     { function = func
                     , argument = arg1
                     , location = ExpressionCursor
                     , kind = TypeKind
                     }
             , argument = arg2
             , location = ExpressionCursor
             , kind = TypeKind
             })

deriving instance Generic (TypeVariable Generated)
instance GenUnchecked (TypeVariable Generated)

deriving instance Generic (TypeConstant Generated)
instance GenUnchecked (TypeConstant Generated)
instance GenValid (TypeConstant Generated)
instance Validity (TypeConstant Generated)

deriving instance Generic TypeName
instance GenUnchecked TypeName
instance GenValid TypeName
instance Validity TypeName

deriving instance Generic Cursor
instance GenUnchecked Cursor
instance GenValid Cursor
instance Validity Cursor

deriving instance Generic Kind
instance GenUnchecked Kind
instance GenValid Kind
instance Validity Kind

deriving instance Generic TypeVariablePrefix
instance GenUnchecked TypeVariablePrefix
instance GenValid TypeVariablePrefix
instance Validity TypeVariablePrefix
