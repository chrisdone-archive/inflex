{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Derived class instances for shared types.

module Inflex.Instances where

import Inflex.Types

--------------------------------------------------------------------------------
-- AST types

deriving instance Show (Expression Parsed)
deriving instance Eq (Expression Parsed)
deriving instance Ord (Expression Parsed)
deriving instance Show (Expression Renamed)
deriving instance Eq (Expression Renamed)
deriving instance Ord (Expression Renamed)
deriving instance Eq (Expression Generated)
deriving instance Ord (Expression Generated)
deriving instance Show (Expression Generated)
deriving instance Eq (Expression Solved)
deriving instance Ord (Expression Solved)
deriving instance Show (Expression Solved)

deriving instance Show (Literal Parsed)
deriving instance Eq (Literal Parsed)
deriving instance Ord (Literal Parsed)
deriving instance Show (Literal Renamed)
deriving instance Eq (Literal Renamed)
deriving instance Ord (Literal Renamed)
deriving instance Eq (Literal Generated)
deriving instance Ord (Literal Generated)
deriving instance Show (Literal Generated)
deriving instance Eq (Literal Solved)
deriving instance Ord (Literal Solved)
deriving instance Show (Literal Solved)

deriving instance Show (Integery Parsed)
deriving instance Eq (Integery Parsed)
deriving instance Ord (Integery Parsed)
deriving instance Show (Integery Renamed)
deriving instance Eq (Integery Renamed)
deriving instance Ord (Integery Renamed)
deriving instance Eq (Integery Generated)
deriving instance Ord (Integery Generated)
deriving instance Show (Integery Generated)
deriving instance Eq (Integery Solved)
deriving instance Ord (Integery Solved)
deriving instance Show (Integery Solved)

deriving instance Show (Lambda Parsed)
deriving instance Eq (Lambda Parsed)
deriving instance Ord (Lambda Parsed)
deriving instance Show (Lambda Renamed)
deriving instance Eq (Lambda Renamed)
deriving instance Ord (Lambda Renamed)
deriving instance Eq (Lambda Generated)
deriving instance Ord (Lambda Generated)
deriving instance Show (Lambda Generated)
deriving instance Eq (Lambda Solved)
deriving instance Ord (Lambda Solved)
deriving instance Show (Lambda Solved)

deriving instance Show (Param Parsed)
deriving instance Eq (Param Parsed)
deriving instance Ord (Param Parsed)
deriving instance Show (Param Renamed)
deriving instance Eq (Param Renamed)
deriving instance Ord (Param Renamed)
deriving instance Eq (Param Generated)
deriving instance Ord (Param Generated)
deriving instance Show (Param Generated)
deriving instance Eq (Param Solved)
deriving instance Ord (Param Solved)
deriving instance Show (Param Solved)

deriving instance Show (Variable Parsed)
deriving instance Eq (Variable Parsed)
deriving instance Ord (Variable Parsed)
deriving instance Show (Variable Renamed)
deriving instance Eq (Variable Renamed)
deriving instance Ord (Variable Renamed)
deriving instance Eq (Variable Generated)
deriving instance Ord (Variable Generated)
deriving instance Show (Variable Generated)
deriving instance Eq (Variable Solved)
deriving instance Ord (Variable Solved)
deriving instance Show (Variable Solved)

deriving instance Show (Apply Parsed)
deriving instance Eq (Apply Parsed)
deriving instance Ord (Apply Parsed)
deriving instance Show (Apply Renamed)
deriving instance Eq (Apply Renamed)
deriving instance Ord (Apply Renamed)
deriving instance Eq (Apply Generated)
deriving instance Ord (Apply Generated)
deriving instance Show (Apply Generated)
deriving instance Eq (Apply Solved)
deriving instance Ord (Apply Solved)
deriving instance Show (Apply Solved)

-------------------------------------------------------------------------------
-- Type system types

deriving instance Show TypeSignature
deriving instance Eq TypeSignature
deriving instance Ord TypeSignature

deriving instance Show EqualityConstraint
deriving instance Eq EqualityConstraint
deriving instance Ord EqualityConstraint

deriving instance Eq (Type Generated)
deriving instance Ord (Type Generated)
deriving instance Show (Type Generated)
deriving instance Eq (Type Solved)
deriving instance Ord (Type Solved)
deriving instance Show (Type Solved)

deriving instance Eq (TypeVariable Generated)
deriving instance Ord (TypeVariable Generated)
deriving instance Show (TypeVariable Generated)
deriving instance Eq (TypeVariable Solved)
deriving instance Ord (TypeVariable Solved)
deriving instance Show (TypeVariable Solved)

deriving instance Eq (TypeApplication Generated)
deriving instance Ord (TypeApplication Generated)
deriving instance Show (TypeApplication Generated)
deriving instance Eq (TypeApplication Solved)
deriving instance Ord (TypeApplication Solved)
deriving instance Show (TypeApplication Solved)

deriving instance Eq (TypeConstant Generated)
deriving instance Ord (TypeConstant Generated)
deriving instance Show (TypeConstant Generated)
deriving instance Eq (TypeConstant Solved)
deriving instance Ord (TypeConstant Solved)
deriving instance Show (TypeConstant Solved)

deriving instance Eq (ClassConstraint Generated)
deriving instance Ord (ClassConstraint Generated)
deriving instance Show (ClassConstraint Generated)
deriving instance Eq (ClassConstraint Solved)
deriving instance Ord (ClassConstraint Solved)
deriving instance Show (ClassConstraint Solved)
