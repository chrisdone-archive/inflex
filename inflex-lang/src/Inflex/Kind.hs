{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

-- | Kind of a type.

module Inflex.Kind where

import Inflex.Types

typeKind :: Type s -> Kind
typeKind =
  \case
    VariableType typeVariable -> typeVariableKind typeVariable
    ApplyType typeApplication -> typeApplicationKind typeApplication
    ConstantType typeConstant -> typeConstantKind typeConstant
    PolyType typePoly -> typeVariableKind typePoly

typeVariableKind :: TypeVariable s -> Kind
typeVariableKind TypeVariable {kind} = kind

typeApplicationKind :: TypeApplication s -> Kind
typeApplicationKind TypeApplication {kind} = kind

typeConstantKind :: TypeConstant s -> Kind
typeConstantKind TypeConstant {name} = typeNameKind name

typeNameKind :: TypeName -> Kind
typeNameKind =
  \case
    IntegerTypeName -> TypeKind
    NaturalTypeName -> TypeKind
    DecimalTypeName{} -> TypeKind -- TODO: Return to this for consideration.
    TextTypeName -> TypeKind
    OptionTypeName -> FunKind TypeKind TypeKind
    FunctionTypeName -> FunKind TypeKind (FunKind TypeKind TypeKind)
