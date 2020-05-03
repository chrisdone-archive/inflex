{-# LANGUAGE GADTs #-}

data Exp env t where
  C :: a -> Exp env a
  V :: Var env t -> Exp env t
  L :: Exp (a,env) b -> Exp env (a-> b)
  A :: Exp env (a-> b) -> Exp env a -> Exp env b

data Var env t where
  VZ :: Var (t, env) t
  VS :: Var env t -> Var (a, env) t

eval :: env -> Exp env t -> t
eval env (V v) = lookp v env
eval _   (C c) = c
eval env (L e) = \x -> eval (x, env) e
eval env (A e1 e2) = (eval env e1) (eval env e2)

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
