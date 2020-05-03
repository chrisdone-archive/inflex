{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase, StandaloneDeriving, GADTs #-}

data a :-> b = a :-> b
data a :=> b = a :=> b

data Type env t where
  -- A constant type. Add kinds?
  C :: a -> Type env a
  -- This correponds to a rigid.
  V :: Var env t -> Type env t
  -- This corresponds to forall. a -> ... a ...
  L :: Type (a,env) b -> Type env (a-> b)
  -- This wouldn't be writable by a user.
  A :: Type env (a-> b) -> Type env a -> Type env b
  -- A free variable.
  Free :: Type env ()
  -- An a -> b type.
  Fun :: Type env a -> Type env b -> Type env (a :-> b)
  -- Class qualification: C x => a
  Qual :: c -> Var env x -> Type env o -> Type env ((c, x) :=> o)

data Var env t where
  VZ :: Var (t, env) t
  VS :: Var env t -> Var (a, env) t
deriving instance Show (Var env t)

eval :: env -> Type env t -> t
eval env (Qual c v t) = (c, lookp v env) :=> eval env t
eval _ Free           = ()
eval env (V v)        = lookp v env
eval _   (C c)        = c
eval env (L e)        = \x -> eval (x, env) e
eval env (A e1 e2)    = (eval env e1) (eval env e2)
eval env (Fun x y)    = eval env x :-> eval env y

lookp :: Var env t -> env -> t
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
