{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators, LambdaCase, StandaloneDeriving, GADTs #-}

data Type env t where
  -- A constant type. Add kinds?
  -- Maybe, (), Int, Either, (->), etc.
  Constructor :: kind -> Type env kind
  -- A free variable.
  -- a
  FreeVariable :: kind -> Type env kind
  -- Type application.
  -- Maybe a
  Application :: Type env (ki :*-> ko) -> Type env ki -> Type env ko
  -- -- A generic variable used in a forall scheme.
  -- -- Rigid variables that will not unify with free variables.
  -- GenericVariable :: Var env t -> Type env t
  -- -- Forall scheme:
  -- -- forall. a -> ... a ...
  -- Forall :: Type (a, env) b -> Type env (Forall a b)
  -- -- This wouldn't be writable by a user.
  -- -- E.g. Show a => a -> String
  -- -- Instantiate with: Int
  -- -- Int -> String
  -- InstantiateForall :: Type env (Forall a b) -> Type env a -> Type env b
  -- -- Class qualification
  -- -- E.g. Show a => a -> String
  -- Qualify :: c -> Var env x -> Type env o -> Type env ((c, x) :=> o)
  -- -- Row types.
  -- -- E.g. { a :: Int, b :: String }
  -- Row :: Row env -> Type env RowKind

-- Kinds
data Forall a b
data a :*-> b = a :*-> b
data RowKind
data a :=> b = a :=> b
data ValueKind=ValueKind

data Row env

data Var env t where
  VZ :: Var (t, env) t
  VS :: Var env t -> Var (a, env) t
deriving instance Show (Var env t)

-- demo :: Type env (Forall ValueKind ValueKind)
-- demo =
--   Forall
--     (Application (Constructor (ValueKind :*-> ValueKind)) (GenericVariable VZ))
