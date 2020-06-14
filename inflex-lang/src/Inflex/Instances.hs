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
deriving instance Eq (Expression Generalised)
deriving instance Ord (Expression Generalised)
deriving instance Show (Expression Generalised)

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
deriving instance Eq (Literal Generalised)
deriving instance Ord (Literal Generalised)
deriving instance Show (Literal Generalised)

deriving instance Show (Number Parsed)
deriving instance Eq (Number Parsed)
deriving instance Ord (Number Parsed)
deriving instance Show (Number Renamed)
deriving instance Eq (Number Renamed)
deriving instance Ord (Number Renamed)
deriving instance Eq (Number Generated)
deriving instance Ord (Number Generated)
deriving instance Show (Number Generated)
deriving instance Eq (Number Solved)
deriving instance Ord (Number Solved)
deriving instance Show (Number Solved)
deriving instance Eq (Number Generalised)
deriving instance Ord (Number Generalised)
deriving instance Show (Number Generalised)

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
deriving instance Eq (Lambda Generalised)
deriving instance Ord (Lambda Generalised)
deriving instance Show (Lambda Generalised)

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
deriving instance Eq (Param Generalised)
deriving instance Ord (Param Generalised)
deriving instance Show (Param Generalised)

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
deriving instance Eq (Variable Generalised)
deriving instance Ord (Variable Generalised)
deriving instance Show (Variable Generalised)

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
deriving instance Eq (Apply Generalised)
deriving instance Ord (Apply Generalised)
deriving instance Show (Apply Generalised)

-------------------------------------------------------------------------------
-- Type system types

deriving instance Eq (Scheme Generalised)
deriving instance Ord (Scheme Generalised)
deriving instance Show (Scheme Generalised)
deriving instance Eq (Scheme Polymorphic)
deriving instance Ord (Scheme Polymorphic)
deriving instance Show (Scheme Polymorphic)

deriving instance Show EqualityConstraint
deriving instance Eq EqualityConstraint
deriving instance Ord EqualityConstraint

deriving instance Eq (Type Generated)
deriving instance Ord (Type Generated)
deriving instance Show (Type Generated)
deriving instance Eq (Type Solved)
deriving instance Ord (Type Solved)
deriving instance Show (Type Solved)
deriving instance Eq (Type Generalised)
deriving instance Ord (Type Generalised)
deriving instance Show (Type Generalised)
deriving instance Eq (Type Polymorphic)
deriving instance Ord (Type Polymorphic)
deriving instance Show (Type Polymorphic)

deriving instance Eq (TypeVariable Generated)
deriving instance Ord (TypeVariable Generated)
deriving instance Show (TypeVariable Generated)
deriving instance Eq (TypeVariable Solved)
deriving instance Ord (TypeVariable Solved)
deriving instance Show (TypeVariable Solved)
deriving instance Eq (TypeVariable Generalised)
deriving instance Ord (TypeVariable Generalised)
deriving instance Show (TypeVariable Generalised)
deriving instance Eq (TypeVariable Polymorphic)
deriving instance Ord (TypeVariable Polymorphic)
deriving instance Show (TypeVariable Polymorphic)

deriving instance Eq (TypeApplication Generated)
deriving instance Ord (TypeApplication Generated)
deriving instance Show (TypeApplication Generated)
deriving instance Eq (TypeApplication Solved)
deriving instance Ord (TypeApplication Solved)
deriving instance Show (TypeApplication Solved)
deriving instance Eq (TypeApplication Generalised)
deriving instance Ord (TypeApplication Generalised)
deriving instance Show (TypeApplication Generalised)
deriving instance Eq (TypeApplication Polymorphic)
deriving instance Ord (TypeApplication Polymorphic)
deriving instance Show (TypeApplication Polymorphic)

deriving instance Eq (TypeConstant Generated)
deriving instance Ord (TypeConstant Generated)
deriving instance Show (TypeConstant Generated)
deriving instance Eq (TypeConstant Solved)
deriving instance Ord (TypeConstant Solved)
deriving instance Show (TypeConstant Solved)
deriving instance Eq (TypeConstant Generalised)
deriving instance Ord (TypeConstant Generalised)
deriving instance Show (TypeConstant Generalised)
deriving instance Eq (TypeConstant Polymorphic)
deriving instance Ord (TypeConstant Polymorphic)
deriving instance Show (TypeConstant Polymorphic)

deriving instance Eq (ClassConstraint Generated)
deriving instance Ord (ClassConstraint Generated)
deriving instance Show (ClassConstraint Generated)
deriving instance Eq (ClassConstraint Solved)
deriving instance Ord (ClassConstraint Solved)
deriving instance Show (ClassConstraint Solved)
deriving instance Eq (ClassConstraint Generalised)
deriving instance Ord (ClassConstraint Generalised)
deriving instance Show (ClassConstraint Generalised)
deriving instance Eq (ClassConstraint Polymorphic)
deriving instance Ord (ClassConstraint Polymorphic)
deriving instance Show (ClassConstraint Polymorphic)
