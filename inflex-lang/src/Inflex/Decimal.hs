{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ExistentialQuantification #-}

-- | Handling of decimals.

module Inflex.Decimal where

import Data.Fixed
import Data.Maybe
import Data.Proxy
import Data.Type.Equality
import GHC.TypeLits
import Inflex.Types
import Numeric.Natural

data Places (n :: Nat)

data SomeFixed =
  forall (p :: Nat). (KnownNat p) => SomeFixed !Natural !(Fixed (Places p))
deriving instance Show SomeFixed

instance KnownNat n => HasResolution (Places n) where
  resolution _ = natVal (Proxy @n)

-- | Set the decimal precision to larger @p@.
expandDecimalPrecision :: Natural -> Decimal -> Decimal
expandDecimalPrecision new Decimal {integer, places = old} =
  Decimal {places = new, integer = integer * (10 ^ (new - old))}

-- | Convert a fixed number back to decimal.
fixedToDecimal :: SomeFixed -> Decimal
fixedToDecimal (SomeFixed places (MkFixed integer :: Fixed (Places n))) =
  Decimal {places, integer}

-- N places becomes a precision of 10^N.
-- > decimalToFixed Decimal{integer=1250, places = 2}
-- SomeFixed 12.50
decimalToFixed :: Decimal -> SomeFixed
decimalToFixed Decimal {places, integer} =
  case someNaturalVal (10 ^ places) of
    SomeNat (_ :: Proxy p) -> SomeFixed @p places (MkFixed integer)

-- | All naturals have a total conversion to SomeNat.
someNaturalVal :: Natural -> SomeNat
someNaturalVal =
  fromMaybe (error "someNaturalVal: impossible occurred. Report as bug.") .
  someNatVal . fromIntegral

-- | If the two fixed numbers are the same precision, do something
-- with them of the same type. Otherwise, return Nothing.
sameFixed ::
     SomeFixed
  -> SomeFixed
  -> (forall p. KnownNat p =>
                  Fixed (Places p) -> Fixed (Places p) -> r)
  -> Maybe r
sameFixed (SomeFixed _ (a :: Fixed (Places a))) (SomeFixed _ (b :: Fixed (Places b))) k =
  case sameNat (Proxy @a) (Proxy @b) of
    Nothing -> Nothing
    Just (Refl :: a :~: b) -> Just (k a b)
