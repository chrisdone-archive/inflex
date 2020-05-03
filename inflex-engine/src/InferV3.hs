{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module InferV3 where

import Data.Finite
import GHC.TypeLits

data Monomorphic
data Polymorphic (n :: Nat)

data a :-> b = a :-> b deriving Show; infixr 2 :->

data Type s k where
  Var :: Integer -> k -> Type s k
  Con :: String -> k -> Type s k
  App :: Type s (i :-> o) -> Type s i -> Type s o
  -- We use a @Finite@ to ensure that any @Gen@ instance is always
  -- within the bounds of the @forall x y z. ...@ -- this is a
  -- Polymorphic 3.
  --
  -- We could even go as far as to use @Polymorphic 0@ to mean "not
  -- polymorphic", but I'm not sure how I feel about that.
  Gen :: Finite n -> k -> Type (Polymorphic n) k

-- If the @Finite@ and Nat in the @Polymorphic@ doesn't work out, we
-- can just drop it.
data SomePolyType k =
  forall n. KnownNat n =>
            SomePolyType (Type (Polymorphic n) k)

-- | Convert a polymorphic type to a monomorphic type.
--
-- In other words, replace all the @Gen@ with @Var@.
mono :: SomePolyType k -> (Type Monomorphic k)
mono (SomePolyType t) = go t
  where go :: Type (Polymorphic n) k -> Type Monomorphic k
        go =
          \case
             -- Here we lookup, or if missing, generate. The function
             -- @genToVar idx k@ should always be idempotent. It should also
             -- check that the returned lookup variable has the same kind;
             -- else that's an error.
             Gen idx k -> undefined
             --
             Var idx k -> Var idx k
             Con s k -> Con s k
             App f x -> App (go f) (go x)

-- | Generalize a monomorphic type to a polymorphic type.
poly :: Type Monomorphic k -> SomePolyType k
poly = undefined
