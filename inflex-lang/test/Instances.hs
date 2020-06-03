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
