{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Derived class instances for shared types.

module Inflex.Instances where

import Data.Hashable
import Inflex.Types
import Language.Haskell.TH.Lift

--------------------------------------------------------------------------------
-- AST types

deriving instance Show (Expression Parsed)
deriving instance Show (Expression Parsed2)
deriving instance Lift (Expression Parsed)
deriving instance Eq (Expression Parsed)
deriving instance Ord (Expression Parsed)
deriving instance Show (Expression Renamed)
deriving instance Lift (Expression Renamed)
deriving instance Eq (Expression Renamed)
deriving instance Ord (Expression Renamed)
deriving instance Eq (Expression Generated)
deriving instance Ord (Expression Generated)
deriving instance Show (Expression Generated)
deriving instance Lift (Expression Generated)
deriving instance Eq (Expression Solved)
deriving instance Ord (Expression Solved)
deriving instance Show (Expression Solved)
deriving instance Lift (Expression Solved)
deriving instance Eq (Expression Generalised)
deriving instance Ord (Expression Generalised)
deriving instance Show (Expression Generalised)
deriving instance Lift (Expression Generalised)
deriving instance Eq (Expression Resolved)
deriving instance Ord (Expression Resolved)
deriving instance Show (Expression Resolved)
deriving instance Lift (Expression Resolved)

deriving instance Show (Record Parsed)
deriving instance Show (Record Parsed2)
deriving instance Lift (Record Parsed)
deriving instance Eq (Record Parsed)
deriving instance Ord (Record Parsed)
deriving instance Show (Record Renamed)
deriving instance Lift (Record Renamed)
deriving instance Eq (Record Renamed)
deriving instance Ord (Record Renamed)
deriving instance Eq (Record Generated)
deriving instance Ord (Record Generated)
deriving instance Show (Record Generated)
deriving instance Lift (Record Generated)
deriving instance Eq (Record Solved)
deriving instance Ord (Record Solved)
deriving instance Show (Record Solved)
deriving instance Lift (Record Solved)
deriving instance Eq (Record Generalised)
deriving instance Ord (Record Generalised)
deriving instance Show (Record Generalised)
deriving instance Lift (Record Generalised)
deriving instance Eq (Record Resolved)
deriving instance Ord (Record Resolved)
deriving instance Show (Record Resolved)
deriving instance Lift (Record Resolved)

deriving instance Show (Hole Parsed)
deriving instance Show (Hole Parsed2)
deriving instance Lift (Hole Parsed)
deriving instance Eq (Hole Parsed)
deriving instance Ord (Hole Parsed)
deriving instance Show (Hole Renamed)
deriving instance Lift (Hole Renamed)
deriving instance Eq (Hole Renamed)
deriving instance Ord (Hole Renamed)
deriving instance Eq (Hole Generated)
deriving instance Ord (Hole Generated)
deriving instance Show (Hole Generated)
deriving instance Lift (Hole Generated)
deriving instance Eq (Hole Solved)
deriving instance Ord (Hole Solved)
deriving instance Show (Hole Solved)
deriving instance Lift (Hole Solved)
deriving instance Eq (Hole Generalised)
deriving instance Ord (Hole Generalised)
deriving instance Show (Hole Generalised)
deriving instance Lift (Hole Generalised)
deriving instance Eq (Hole Resolved)
deriving instance Ord (Hole Resolved)
deriving instance Show (Hole Resolved)
deriving instance Lift (Hole Resolved)

deriving instance Show (Variant Parsed)
deriving instance Show (Variant Parsed2)
deriving instance Lift (Variant Parsed)
deriving instance Eq (Variant Parsed)
deriving instance Ord (Variant Parsed)
deriving instance Show (Variant Renamed)
deriving instance Lift (Variant Renamed)
deriving instance Eq (Variant Renamed)
deriving instance Ord (Variant Renamed)
deriving instance Eq (Variant Generated)
deriving instance Ord (Variant Generated)
deriving instance Show (Variant Generated)
deriving instance Lift (Variant Generated)
deriving instance Eq (Variant Solved)
deriving instance Ord (Variant Solved)
deriving instance Show (Variant Solved)
deriving instance Lift (Variant Solved)
deriving instance Eq (Variant Generalised)
deriving instance Ord (Variant Generalised)
deriving instance Show (Variant Generalised)
deriving instance Lift (Variant Generalised)
deriving instance Eq (Variant Resolved)
deriving instance Ord (Variant Resolved)
deriving instance Show (Variant Resolved)
deriving instance Lift (Variant Resolved)

deriving instance Show (Prop Parsed)
deriving instance Show (Prop Parsed2)
deriving instance Lift (Prop Parsed)
deriving instance Eq (Prop Parsed)
deriving instance Ord (Prop Parsed)
deriving instance Show (Prop Renamed)
deriving instance Lift (Prop Renamed)
deriving instance Eq (Prop Renamed)
deriving instance Ord (Prop Renamed)
deriving instance Eq (Prop Generated)
deriving instance Ord (Prop Generated)
deriving instance Show (Prop Generated)
deriving instance Lift (Prop Generated)
deriving instance Eq (Prop Solved)
deriving instance Ord (Prop Solved)
deriving instance Show (Prop Solved)
deriving instance Lift (Prop Solved)
deriving instance Eq (Prop Generalised)
deriving instance Ord (Prop Generalised)
deriving instance Show (Prop Generalised)
deriving instance Lift (Prop Generalised)
deriving instance Eq (Prop Resolved)
deriving instance Ord (Prop Resolved)
deriving instance Show (Prop Resolved)
deriving instance Lift (Prop Resolved)

deriving instance Show (Array Parsed)
deriving instance Show (Array Parsed2)
deriving instance Lift (Array Parsed)
deriving instance Eq (Array Parsed)
deriving instance Ord (Array Parsed)
deriving instance Show (Array Renamed)
deriving instance Lift (Array Renamed)
deriving instance Eq (Array Renamed)
deriving instance Ord (Array Renamed)
deriving instance Eq (Array Generated)
deriving instance Ord (Array Generated)
deriving instance Show (Array Generated)
deriving instance Lift (Array Generated)
deriving instance Eq (Array Solved)
deriving instance Ord (Array Solved)
deriving instance Show (Array Solved)
deriving instance Lift (Array Solved)
deriving instance Eq (Array Generalised)
deriving instance Ord (Array Generalised)
deriving instance Show (Array Generalised)
deriving instance Lift (Array Generalised)
deriving instance Eq (Array Resolved)
deriving instance Ord (Array Resolved)
deriving instance Show (Array Resolved)
deriving instance Lift (Array Resolved)

deriving instance Show (TermForm)
deriving instance Lift (TermForm)
deriving instance Eq (TermForm)
deriving instance Ord (TermForm)

deriving instance Show (FieldE Parsed)
deriving instance Show (FieldE Parsed2)
deriving instance Lift (FieldE Parsed)
deriving instance Eq (FieldE Parsed)
deriving instance Ord (FieldE Parsed)
deriving instance Show (FieldE Renamed)
deriving instance Lift (FieldE Renamed)
deriving instance Eq (FieldE Renamed)
deriving instance Ord (FieldE Renamed)
deriving instance Eq (FieldE Generated)
deriving instance Ord (FieldE Generated)
deriving instance Show (FieldE Generated)
deriving instance Lift (FieldE Generated)
deriving instance Eq (FieldE Solved)
deriving instance Ord (FieldE Solved)
deriving instance Show (FieldE Solved)
deriving instance Lift (FieldE Solved)
deriving instance Eq (FieldE Generalised)
deriving instance Ord (FieldE Generalised)
deriving instance Show (FieldE Generalised)
deriving instance Lift (FieldE Generalised)
deriving instance Eq (FieldE Resolved)
deriving instance Ord (FieldE Resolved)
deriving instance Show (FieldE Resolved)
deriving instance Lift (FieldE Resolved)

deriving instance Show (Literal Parsed)
deriving instance Show (Literal Parsed2)
deriving instance Lift (Literal Parsed)
deriving instance Eq (Literal Parsed)
deriving instance Ord (Literal Parsed)
deriving instance Show (Literal Renamed)
deriving instance Lift (Literal Renamed)
deriving instance Eq (Literal Renamed)
deriving instance Ord (Literal Renamed)
deriving instance Eq (Literal Generated)
deriving instance Ord (Literal Generated)
deriving instance Show (Literal Generated)
deriving instance Lift (Literal Generated)
deriving instance Eq (Literal Solved)
deriving instance Ord (Literal Solved)
deriving instance Show (Literal Solved)
deriving instance Lift (Literal Solved)
deriving instance Eq (Literal Generalised)
deriving instance Ord (Literal Generalised)
deriving instance Show (Literal Generalised)
deriving instance Lift (Literal Generalised)
deriving instance Eq (Literal Resolved)
deriving instance Ord (Literal Resolved)
deriving instance Show (Literal Resolved)
deriving instance Lift (Literal Resolved)

deriving instance Show (LiteralText Parsed)
deriving instance Show (LiteralText Parsed2)
deriving instance Lift (LiteralText Parsed)
deriving instance Eq (LiteralText Parsed)
deriving instance Ord (LiteralText Parsed)
deriving instance Show (LiteralText Renamed)
deriving instance Lift (LiteralText Renamed)
deriving instance Eq (LiteralText Renamed)
deriving instance Ord (LiteralText Renamed)
deriving instance Eq (LiteralText Generated)
deriving instance Ord (LiteralText Generated)
deriving instance Show (LiteralText Generated)
deriving instance Lift (LiteralText Generated)
deriving instance Eq (LiteralText Solved)
deriving instance Ord (LiteralText Solved)
deriving instance Show (LiteralText Solved)
deriving instance Lift (LiteralText Solved)
deriving instance Eq (LiteralText Generalised)
deriving instance Ord (LiteralText Generalised)
deriving instance Show (LiteralText Generalised)
deriving instance Lift (LiteralText Generalised)
deriving instance Eq (LiteralText Resolved)
deriving instance Ord (LiteralText Resolved)
deriving instance Show (LiteralText Resolved)
deriving instance Lift (LiteralText Resolved)

deriving instance Show (Number Parsed)
deriving instance Show (Number Parsed2)
deriving instance Lift (Number Parsed)
deriving instance Eq (Number Parsed)
deriving instance Ord (Number Parsed)
deriving instance Show (Number Renamed)
deriving instance Lift (Number Renamed)
deriving instance Eq (Number Renamed)
deriving instance Ord (Number Renamed)
deriving instance Eq (Number Generated)
deriving instance Ord (Number Generated)
deriving instance Show (Number Generated)
deriving instance Lift (Number Generated)
deriving instance Eq (Number Solved)
deriving instance Ord (Number Solved)
deriving instance Show (Number Solved)
deriving instance Lift (Number Solved)
deriving instance Eq (Number Generalised)
deriving instance Ord (Number Generalised)
deriving instance Show (Number Generalised)
deriving instance Lift (Number Generalised)
deriving instance Eq (Number Resolved)
deriving instance Ord (Number Resolved)
deriving instance Show (Number Resolved)
deriving instance Lift (Number Resolved)

deriving instance Show (Lambda Parsed)
deriving instance Show (Lambda Parsed2)
deriving instance Lift (Lambda Parsed)
deriving instance Eq (Lambda Parsed)
deriving instance Ord (Lambda Parsed)
deriving instance Show (Lambda Renamed)
deriving instance Lift (Lambda Renamed)
deriving instance Eq (Lambda Renamed)
deriving instance Ord (Lambda Renamed)
deriving instance Eq (Lambda Generated)
deriving instance Ord (Lambda Generated)
deriving instance Show (Lambda Generated)
deriving instance Lift (Lambda Generated)
deriving instance Eq (Lambda Solved)
deriving instance Ord (Lambda Solved)
deriving instance Show (Lambda Solved)
deriving instance Lift (Lambda Solved)
deriving instance Eq (Lambda Generalised)
deriving instance Ord (Lambda Generalised)
deriving instance Show (Lambda Generalised)
deriving instance Lift (Lambda Generalised)
deriving instance Eq (Lambda Resolved)
deriving instance Ord (Lambda Resolved)
deriving instance Show (Lambda Resolved)
deriving instance Lift (Lambda Resolved)

deriving instance Show (Infix Parsed)
deriving instance Show (Infix Parsed2)
deriving instance Lift (Infix Parsed)
deriving instance Eq (Infix Parsed)
deriving instance Ord (Infix Parsed)
deriving instance Show (Infix Renamed)
deriving instance Lift (Infix Renamed)
deriving instance Eq (Infix Renamed)
deriving instance Ord (Infix Renamed)
deriving instance Eq (Infix Generated)
deriving instance Ord (Infix Generated)
deriving instance Show (Infix Generated)
deriving instance Lift (Infix Generated)
deriving instance Eq (Infix Solved)
deriving instance Ord (Infix Solved)
deriving instance Show (Infix Solved)
deriving instance Lift (Infix Solved)
deriving instance Eq (Infix Generalised)
deriving instance Ord (Infix Generalised)
deriving instance Show (Infix Generalised)
deriving instance Lift (Infix Generalised)
deriving instance Eq (Infix Resolved)
deriving instance Ord (Infix Resolved)
deriving instance Show (Infix Resolved)
deriving instance Lift (Infix Resolved)

deriving instance Show (Param Parsed)
deriving instance Show (Param Parsed2)
deriving instance Lift (Param Parsed)
deriving instance Eq (Param Parsed)
deriving instance Ord (Param Parsed)
deriving instance Show (Param Renamed)
deriving instance Lift (Param Renamed)
deriving instance Eq (Param Renamed)
deriving instance Ord (Param Renamed)
deriving instance Eq (Param Generated)
deriving instance Ord (Param Generated)
deriving instance Show (Param Generated)
deriving instance Lift (Param Generated)
deriving instance Eq (Param Solved)
deriving instance Ord (Param Solved)
deriving instance Show (Param Solved)
deriving instance Lift (Param Solved)
deriving instance Eq (Param Generalised)
deriving instance Ord (Param Generalised)
deriving instance Show (Param Generalised)
deriving instance Lift (Param Generalised)
deriving instance Eq (Param Resolved)
deriving instance Ord (Param Resolved)
deriving instance Show (Param Resolved)
deriving instance Lift (Param Resolved)

deriving instance Show (Variable Parsed)
deriving instance Show (Variable Parsed2)
deriving instance Lift (Variable Parsed)
deriving instance Eq (Variable Parsed)
deriving instance Ord (Variable Parsed)
deriving instance Show (Variable Renamed)
deriving instance Lift (Variable Renamed)
deriving instance Eq (Variable Renamed)
deriving instance Ord (Variable Renamed)
deriving instance Show (Variable Filled)
deriving instance Lift (Variable Filled)
deriving instance Eq (Variable Filled)
deriving instance Ord (Variable Filled)
deriving instance Eq (Variable Generated)
deriving instance Ord (Variable Generated)
deriving instance Show (Variable Generated)
deriving instance Lift (Variable Generated)
deriving instance Eq (Variable Solved)
deriving instance Ord (Variable Solved)
deriving instance Show (Variable Solved)
deriving instance Lift (Variable Solved)
deriving instance Eq (Variable Generalised)
deriving instance Ord (Variable Generalised)
deriving instance Show (Variable Generalised)
deriving instance Lift (Variable Generalised)
deriving instance Eq (Variable Resolved)
deriving instance Ord (Variable Resolved)
deriving instance Show (Variable Resolved)
deriving instance Lift (Variable Resolved)

deriving instance Show (Global Parsed)
deriving instance Show (Global Parsed2)
deriving instance Lift (Global Parsed)
deriving instance Eq (Global Parsed)
deriving instance Ord (Global Parsed)
deriving instance Show (Global Renamed)
deriving instance Lift (Global Renamed)
deriving instance Eq (Global Renamed)
deriving instance Ord (Global Renamed)
deriving instance Eq (Global Generated)
deriving instance Ord (Global Generated)
deriving instance Show (Global Generated)
deriving instance Lift (Global Generated)
deriving instance Eq (Global Solved)
deriving instance Ord (Global Solved)
deriving instance Show (Global Solved)
deriving instance Lift (Global Solved)
deriving instance Eq (Global Generalised)
deriving instance Ord (Global Generalised)
deriving instance Show (Global Generalised)
deriving instance Lift (Global Generalised)
deriving instance Eq (Global Resolved)
deriving instance Ord (Global Resolved)
deriving instance Show (Global Resolved)
deriving instance Lift (Global Resolved)

deriving instance Show (Apply Parsed)
deriving instance Show (Apply Parsed2)
deriving instance Lift (Apply Parsed)
deriving instance Eq (Apply Parsed)
deriving instance Ord (Apply Parsed)
deriving instance Show (Apply Renamed)
deriving instance Lift (Apply Renamed)
deriving instance Eq (Apply Renamed)
deriving instance Ord (Apply Renamed)
deriving instance Eq (Apply Generated)
deriving instance Ord (Apply Generated)
deriving instance Show (Apply Generated)
deriving instance Lift (Apply Generated)
deriving instance Eq (Apply Solved)
deriving instance Ord (Apply Solved)
deriving instance Show (Apply Solved)
deriving instance Lift (Apply Solved)
deriving instance Eq (Apply Generalised)
deriving instance Ord (Apply Generalised)
deriving instance Show (Apply Generalised)
deriving instance Lift (Apply Generalised)
deriving instance Eq (Apply Resolved)
deriving instance Ord (Apply Resolved)
deriving instance Show (Apply Resolved)
deriving instance Lift (Apply Resolved)

-------------------------------------------------------------------------------
-- Type system types

deriving instance Eq (Scheme Generated)
deriving instance Ord (Scheme Generated)
deriving instance Show (Scheme Generated)
deriving instance Lift (Scheme Generated)
deriving instance Eq (Scheme Solved)
deriving instance Ord (Scheme Solved)
deriving instance Show (Scheme Solved)
deriving instance Lift (Scheme Solved)
deriving instance Eq (Scheme Generalised)
deriving instance Ord (Scheme Generalised)
deriving instance Show (Scheme Generalised)
deriving instance Lift (Scheme Generalised)
deriving instance Eq (Scheme Polymorphic)
deriving instance Ord (Scheme Polymorphic)
deriving instance Show (Scheme Polymorphic)
deriving instance Lift (Scheme Polymorphic)

deriving instance Show EqualityConstraint
deriving instance Lift EqualityConstraint
deriving instance Eq EqualityConstraint
deriving instance Ord EqualityConstraint

deriving instance Eq (Type Parsed)
deriving instance Ord (Type Parsed)
deriving instance Show (Type Parsed)
deriving instance Show (Type Parsed2)
deriving instance Lift (Type Parsed)
deriving instance Eq (Type Renamed)
deriving instance Ord (Type Renamed)
deriving instance Show (Type Renamed)
deriving instance Lift (Type Renamed)
deriving instance Eq (Type Generated)
deriving instance Ord (Type Generated)
deriving instance Show (Type Generated)
deriving instance Lift (Type Generated)
deriving instance Eq (Type Solved)
deriving instance Ord (Type Solved)
deriving instance Show (Type Solved)
deriving instance Lift (Type Solved)
deriving instance Eq (Type Generalised)
deriving instance Ord (Type Generalised)
deriving instance Show (Type Generalised)
deriving instance Lift (Type Generalised)
deriving instance Eq (Type Polymorphic)
deriving instance Ord (Type Polymorphic)
deriving instance Show (Type Polymorphic)
deriving instance Lift (Type Polymorphic)

deriving instance Eq (TypeVariable Generated)
instance Hashable (TypeVariable Generated) where
  hashWithSalt salt TypeVariable {index,prefix} = hashWithSalt salt (index, prefix)
deriving instance Ord (TypeVariable Generated)
deriving instance Show (TypeVariable Generated)
deriving instance Lift (TypeVariable Generated)
instance Eq (TypeVariable Parsed) where
  (==) TypeVariable {index,prefix} TypeVariable {index=index2,prefix=prefix2} = (index,prefix)== (index2,prefix2)
deriving instance Ord (TypeVariable Parsed)
deriving instance Show (TypeVariable Parsed)
deriving instance Show (TypeVariable Parsed2)
deriving instance Lift (TypeVariable Parsed)
deriving instance Eq (TypeVariable Renamed)
deriving instance Ord (TypeVariable Renamed)
deriving instance Show (TypeVariable Renamed)
deriving instance Lift (TypeVariable Renamed)
deriving instance Eq (TypeVariable Solved)
deriving instance Ord (TypeVariable Solved)
deriving instance Show (TypeVariable Solved)
deriving instance Lift (TypeVariable Solved)
deriving instance Eq (TypeVariable Generalised)
deriving instance Ord (TypeVariable Generalised)
deriving instance Show (TypeVariable Generalised)
deriving instance Lift (TypeVariable Generalised)
deriving instance Eq (TypeVariable Polymorphic)
deriving instance Ord (TypeVariable Polymorphic)
deriving instance Show (TypeVariable Polymorphic)
deriving instance Lift (TypeVariable Polymorphic)

deriving instance Eq (TypeApplication Generated)
deriving instance Ord (TypeApplication Generated)
deriving instance Show (TypeApplication Generated)
deriving instance Lift (TypeApplication Generated)
deriving instance Eq (TypeApplication Parsed)
deriving instance Ord (TypeApplication Parsed)
deriving instance Show (TypeApplication Parsed)
deriving instance Show (TypeApplication Parsed2)
deriving instance Lift (TypeApplication Parsed)
deriving instance Eq (TypeApplication Renamed)
deriving instance Ord (TypeApplication Renamed)
deriving instance Show (TypeApplication Renamed)
deriving instance Lift (TypeApplication Renamed)
deriving instance Eq (TypeApplication Solved)
deriving instance Ord (TypeApplication Solved)
deriving instance Show (TypeApplication Solved)
deriving instance Lift (TypeApplication Solved)
deriving instance Eq (TypeApplication Generalised)
deriving instance Ord (TypeApplication Generalised)
deriving instance Show (TypeApplication Generalised)
deriving instance Lift (TypeApplication Generalised)
deriving instance Eq (TypeApplication Polymorphic)
deriving instance Ord (TypeApplication Polymorphic)
deriving instance Show (TypeApplication Polymorphic)
deriving instance Lift (TypeApplication Polymorphic)

deriving instance Eq (TypeConstant Generated)
deriving instance Ord (TypeConstant Generated)
deriving instance Show (TypeConstant Generated)
deriving instance Lift (TypeConstant Generated)
deriving instance Eq (TypeConstant Parsed)
deriving instance Ord (TypeConstant Parsed)
deriving instance Show (TypeConstant Parsed)
deriving instance Show (TypeConstant Parsed2)
deriving instance Lift (TypeConstant Parsed)
deriving instance Eq (TypeConstant Renamed)
deriving instance Ord (TypeConstant Renamed)
deriving instance Show (TypeConstant Renamed)
deriving instance Lift (TypeConstant Renamed)
deriving instance Eq (TypeConstant Solved)
deriving instance Ord (TypeConstant Solved)
deriving instance Show (TypeConstant Solved)
deriving instance Lift (TypeConstant Solved)
deriving instance Eq (TypeConstant Generalised)
deriving instance Ord (TypeConstant Generalised)
deriving instance Show (TypeConstant Generalised)
deriving instance Lift (TypeConstant Generalised)
deriving instance Eq (TypeConstant Polymorphic)
deriving instance Ord (TypeConstant Polymorphic)
deriving instance Show (TypeConstant Polymorphic)
deriving instance Lift (TypeConstant Polymorphic)

deriving instance Eq (TypeRow Generated)
deriving instance Ord (TypeRow Generated)
deriving instance Show (TypeRow Generated)
deriving instance Lift (TypeRow Generated)
deriving instance Eq (TypeRow Parsed)
deriving instance Ord (TypeRow Parsed)
deriving instance Show (TypeRow Parsed)
deriving instance Show (TypeRow Parsed2)
deriving instance Lift (TypeRow Parsed)
deriving instance Eq (TypeRow Renamed)
deriving instance Ord (TypeRow Renamed)
deriving instance Show (TypeRow Renamed)
deriving instance Lift (TypeRow Renamed)
deriving instance Eq (TypeRow Solved)
deriving instance Ord (TypeRow Solved)
deriving instance Show (TypeRow Solved)
deriving instance Lift (TypeRow Solved)
deriving instance Eq (TypeRow Generalised)
deriving instance Ord (TypeRow Generalised)
deriving instance Show (TypeRow Generalised)
deriving instance Lift (TypeRow Generalised)
deriving instance Eq (TypeRow Polymorphic)
deriving instance Ord (TypeRow Polymorphic)
deriving instance Show (TypeRow Polymorphic)
deriving instance Lift (TypeRow Polymorphic)

deriving instance Eq (Field Generated)
deriving instance Ord (Field Generated)
deriving instance Show (Field Generated)
deriving instance Lift (Field Generated)
deriving instance Eq (Field Parsed)
deriving instance Ord (Field Parsed)
deriving instance Show (Field Parsed)
deriving instance Show (Field Parsed2)
deriving instance Lift (Field Parsed)
deriving instance Eq (Field Renamed)
deriving instance Ord (Field Renamed)
deriving instance Show (Field Renamed)
deriving instance Lift (Field Renamed)
deriving instance Eq (Field Solved)
deriving instance Ord (Field Solved)
deriving instance Show (Field Solved)
deriving instance Lift (Field Solved)
deriving instance Eq (Field Generalised)
deriving instance Ord (Field Generalised)
deriving instance Show (Field Generalised)
deriving instance Lift (Field Generalised)
deriving instance Eq (Field Polymorphic)
deriving instance Ord (Field Polymorphic)
deriving instance Show (Field Polymorphic)
deriving instance Lift (Field Polymorphic)

deriving instance Eq (ClassConstraint Generated)
deriving instance Ord (ClassConstraint Generated)
deriving instance Show (ClassConstraint Generated)
deriving instance Lift (ClassConstraint Generated)
deriving instance Eq (ClassConstraint Solved)
deriving instance Ord (ClassConstraint Solved)
deriving instance Show (ClassConstraint Solved)
deriving instance Lift (ClassConstraint Solved)
deriving instance Eq (ClassConstraint Generalised)
deriving instance Ord (ClassConstraint Generalised)
deriving instance Show (ClassConstraint Generalised)
deriving instance Lift (ClassConstraint Generalised)
deriving instance Eq (ClassConstraint Polymorphic)
deriving instance Ord (ClassConstraint Polymorphic)
deriving instance Show (ClassConstraint Polymorphic)
deriving instance Lift (ClassConstraint Polymorphic)

deriving instance Show (StagedScheme Parsed)
deriving instance Show (StagedScheme Parsed2)
deriving instance Lift (StagedScheme Parsed)
deriving instance Eq (StagedScheme Parsed)
deriving instance Ord (StagedScheme Parsed)
deriving instance Show (StagedScheme Renamed)
deriving instance Lift (StagedScheme Renamed)
deriving instance Eq (StagedScheme Renamed)
deriving instance Ord (StagedScheme Renamed)
deriving instance Show (StagedScheme Solved)
deriving instance Lift (StagedScheme Solved)
deriving instance Eq (StagedScheme Solved)
deriving instance Ord (StagedScheme Solved)
deriving instance Show (StagedScheme Generalised)
deriving instance Lift (StagedScheme Generalised)
deriving instance Eq (StagedScheme Generalised)
deriving instance Ord (StagedScheme Generalised)
deriving instance Show (StagedScheme Generated)
deriving instance Lift (StagedScheme Generated)
deriving instance Eq (StagedScheme Generated)
deriving instance Ord (StagedScheme Generated)
deriving instance Show (StagedScheme Resolved)
deriving instance Lift (StagedScheme Resolved)
deriving instance Eq (StagedScheme Resolved)
deriving instance Ord (StagedScheme Resolved)

deriving instance Show (GlobalRef Parsed)
deriving instance Show (GlobalRef Parsed2)
deriving instance Lift (GlobalRef Parsed)
deriving instance Eq (GlobalRef Parsed)
deriving instance Ord (GlobalRef Parsed)
deriving instance Show (GlobalRef Renamed)
deriving instance Lift (GlobalRef Renamed)
deriving instance Eq (GlobalRef Renamed)
deriving instance Ord (GlobalRef Renamed)
deriving instance Show (GlobalRef Solved)
deriving instance Lift (GlobalRef Solved)
deriving instance Eq (GlobalRef Solved)
deriving instance Ord (GlobalRef Solved)
deriving instance Show (GlobalRef Generalised)
deriving instance Lift (GlobalRef Generalised)
deriving instance Eq (GlobalRef Generalised)
deriving instance Ord (GlobalRef Generalised)
deriving instance Show (GlobalRef Generated)
deriving instance Lift (GlobalRef Generated)
deriving instance Eq (GlobalRef Generated)
deriving instance Ord (GlobalRef Generated)
deriving instance Show (GlobalRef Resolved)
deriving instance Lift (GlobalRef Resolved)
deriving instance Eq (GlobalRef Resolved)
deriving instance Ord (GlobalRef Resolved)

deriving instance Show (Binding Parsed)
deriving instance Show (Binding Parsed2)
deriving instance Lift (Binding Parsed)
deriving instance Eq (Binding Parsed)
deriving instance Ord (Binding Parsed)
deriving instance Show (Binding Renamed)
deriving instance Lift (Binding Renamed)
deriving instance Eq (Binding Renamed)
deriving instance Ord (Binding Renamed)
deriving instance Show (Binding Solved)
deriving instance Lift (Binding Solved)
deriving instance Eq (Binding Solved)
deriving instance Ord (Binding Solved)
deriving instance Show (Binding Generalised)
deriving instance Lift (Binding Generalised)
deriving instance Eq (Binding Generalised)
deriving instance Ord (Binding Generalised)
deriving instance Show (Binding Generated)
deriving instance Lift (Binding Generated)
deriving instance Eq (Binding Generated)
deriving instance Ord (Binding Generated)
deriving instance Show (Binding Resolved)
deriving instance Lift (Binding Resolved)
deriving instance Eq (Binding Resolved)
deriving instance Ord (Binding Resolved)

deriving instance Show (Case Parsed)
deriving instance Show (Case Parsed2)
deriving instance Lift (Case Parsed)
deriving instance Eq (Case Parsed)
deriving instance Ord (Case Parsed)
deriving instance Show (Case Renamed)
deriving instance Lift (Case Renamed)
deriving instance Eq (Case Renamed)
deriving instance Ord (Case Renamed)
deriving instance Show (Case Solved)
deriving instance Lift (Case Solved)
deriving instance Eq (Case Solved)
deriving instance Ord (Case Solved)
deriving instance Show (Case Generalised)
deriving instance Lift (Case Generalised)
deriving instance Eq (Case Generalised)
deriving instance Ord (Case Generalised)
deriving instance Show (Case Generated)
deriving instance Lift (Case Generated)
deriving instance Eq (Case Generated)
deriving instance Ord (Case Generated)
deriving instance Show (Case Resolved)
deriving instance Lift (Case Resolved)
deriving instance Eq (Case Resolved)
deriving instance Ord (Case Resolved)

deriving instance Show (VariantP Parsed)
deriving instance Show (VariantP Parsed2)
deriving instance Lift (VariantP Parsed)
deriving instance Eq (VariantP Parsed)
deriving instance Ord (VariantP Parsed)
deriving instance Show (VariantP Renamed)
deriving instance Lift (VariantP Renamed)
deriving instance Eq (VariantP Renamed)
deriving instance Ord (VariantP Renamed)
deriving instance Show (VariantP Solved)
deriving instance Lift (VariantP Solved)
deriving instance Eq (VariantP Solved)
deriving instance Ord (VariantP Solved)
deriving instance Show (VariantP Generalised)
deriving instance Lift (VariantP Generalised)
deriving instance Eq (VariantP Generalised)
deriving instance Ord (VariantP Generalised)
deriving instance Show (VariantP Generated)
deriving instance Lift (VariantP Generated)
deriving instance Eq (VariantP Generated)
deriving instance Ord (VariantP Generated)
deriving instance Show (VariantP Resolved)
deriving instance Lift (VariantP Resolved)
deriving instance Eq (VariantP Resolved)
deriving instance Ord (VariantP Resolved)

deriving instance Show (Alternative Parsed)
deriving instance Show (Alternative Parsed2)
deriving instance Lift (Alternative Parsed)
deriving instance Eq (Alternative Parsed)
deriving instance Ord (Alternative Parsed)
deriving instance Show (Alternative Renamed)
deriving instance Lift (Alternative Renamed)
deriving instance Eq (Alternative Renamed)
deriving instance Ord (Alternative Renamed)
deriving instance Show (Alternative Solved)
deriving instance Lift (Alternative Solved)
deriving instance Eq (Alternative Solved)
deriving instance Ord (Alternative Solved)
deriving instance Show (Alternative Generalised)
deriving instance Lift (Alternative Generalised)
deriving instance Eq (Alternative Generalised)
deriving instance Ord (Alternative Generalised)
deriving instance Show (Alternative Generated)
deriving instance Lift (Alternative Generated)
deriving instance Eq (Alternative Generated)
deriving instance Ord (Alternative Generated)
deriving instance Show (Alternative Resolved)
deriving instance Lift (Alternative Resolved)
deriving instance Eq (Alternative Resolved)
deriving instance Ord (Alternative Resolved)

deriving instance Show (Pattern Parsed)
deriving instance Show (Pattern Parsed2)
deriving instance Lift (Pattern Parsed)
deriving instance Eq (Pattern Parsed)
deriving instance Ord (Pattern Parsed)
deriving instance Show (Pattern Renamed)
deriving instance Lift (Pattern Renamed)
deriving instance Eq (Pattern Renamed)
deriving instance Ord (Pattern Renamed)
deriving instance Show (Pattern Solved)
deriving instance Lift (Pattern Solved)
deriving instance Eq (Pattern Solved)
deriving instance Ord (Pattern Solved)
deriving instance Show (Pattern Generalised)
deriving instance Lift (Pattern Generalised)
deriving instance Eq (Pattern Generalised)
deriving instance Ord (Pattern Generalised)
deriving instance Show (Pattern Generated)
deriving instance Lift (Pattern Generated)
deriving instance Eq (Pattern Generated)
deriving instance Ord (Pattern Generated)
deriving instance Show (Pattern Resolved)
deriving instance Lift (Pattern Resolved)
deriving instance Eq (Pattern Resolved)
deriving instance Ord (Pattern Resolved)

deriving instance Eq Cell
deriving instance Ord Cell
deriving instance Show Cell
deriving instance Lift Cell

deriving instance Eq IncompleteGlobalRef
deriving instance Ord IncompleteGlobalRef
deriving instance Show IncompleteGlobalRef
deriving instance Lift IncompleteGlobalRef

deriving instance Eq ParsedGlobal
deriving instance Ord ParsedGlobal
deriving instance Show ParsedGlobal
deriving instance Lift ParsedGlobal

deriving instance Eq CellRefStyle
deriving instance Ord CellRefStyle
deriving instance Show CellRefStyle
deriving instance Lift CellRefStyle

deriving instance Eq CellAddress
deriving instance Ord CellAddress
deriving instance Show CellAddress
deriving instance Lift CellAddress

deriving instance Eq CellRef
deriving instance Ord CellRef
deriving instance Show CellRef
deriving instance Lift CellRef

deriving instance Eq (Default Polymorphic)
deriving instance Ord (Default Polymorphic)
deriving instance Show (Default Polymorphic)
deriving instance Lift (Default Polymorphic)
