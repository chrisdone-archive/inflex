-- This one separates the kind from the return @a@ value.

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase, StandaloneDeriving, GADTs #-}

data Type env k a where
 -- Regular types

  -- A constant type.
  C :: Show k => k -> Type env k ()
  -- A free variable.
  Free :: Show k => k -> Type env k ()
  -- App.
  Ap :: Type env (i :-> k) () -> Type env i () -> Type env k ()

  -- Schemes

  -- This correponds to a rigid.
  V :: Var env k t -> Type env k t
  -- This corresponds to forall a. ... a ...
  L :: Type (a,env) o b -> Type env (Forall i o) (a -> b)
  -- This wouldn't be writable by a user.
  A :: Type env (Forall k o) (a -> b) -> Type env k a -> Type env o b
deriving instance Show (Type env k a)

data Forall k o = Forall k o deriving (Show)
data TypeK = TypeK deriving (Show)
data a :-> k = a :-> k deriving (Show)

data Var env k t where
  VZ :: Var (t, env) k t
  VS :: Var env k t -> Var (a, env) k t
deriving instance Show (Var env k t)

eval :: env -> Type env k t -> t
eval _ Ap {}          = ()
eval _ Free{}         = ()
eval env (V v)        = lookp v env
eval _   C{}          = ()
eval env (L e)        = \x -> eval (x, env) e
eval env (A e1 e2)    = (eval env e1) (eval env e2)

lookp :: Var env k t -> env -> t
lookp VZ (x,_)        = x
lookp (VS v) (_, env) = lookp v env

{-

Example:

> eval () (A (L (V VZ)) (C 'a'))
'a'

-- const
> eval () (A (A (L (L (V (VS VZ)))) (C 'a')) (C 123))
'a'

-}
