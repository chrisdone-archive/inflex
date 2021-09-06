{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs, PolyKinds #-}
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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveLift #-}

-- | A dynamic-precision decimal type. The precision is fixed, but
-- only known at runtime, making it easier to work with them.
--
-- Highly inefficient, but easy to make efficient later.

module Data.Decimal
  ( Decimal(..)
  , plus
  , minus
  , multiply
  , divide
  , decimalFromInteger
  , expandDecimalPrecision
  , someNaturalVal
  , showDecimal
  ) where

import           Data.Fixed
import           Data.Maybe
import           Data.Proxy
import           Data.String
import           GHC.Natural
import           GHC.TypeLits
import           Language.Haskell.TH.Syntax
import qualified RIO

--------------------------------------------------------------------------------
-- Types

-- | Decimal backed by an Integer with N decimal places. Precision is
-- determined at runtime.
data Decimal = Decimal
  { places :: !Natural
  , integer :: !Integer
  } deriving (Show, Lift, Eq, Ord)

instance RIO.Display Decimal where
  display = fromString . showDecimal

--------------------------------------------------------------------------------
-- Operations

plus :: Decimal -> Decimal -> Decimal
plus Decimal {integer = a} Decimal {integer = b, ..} =
  Decimal {integer = a + b, ..}

minus :: Decimal -> Decimal -> Decimal
minus Decimal {integer = a} Decimal {integer = b, ..} =
  Decimal {integer = a - b, ..}

multiply :: Decimal -> Decimal -> Decimal
multiply Decimal {integer = a} Decimal {integer = b, ..} =
  Decimal {integer = div (a * b) (fromIntegral (placesToResolution places)), ..}

divide :: Decimal -> Decimal -> Decimal
divide Decimal {integer = a} Decimal {integer = b, ..} =
  Decimal {integer = div (a * (fromIntegral (placesToResolution places))) b, ..}

-- | Convert an integer to a decimal of @places@.
decimalFromInteger :: Integer -> Natural -> Decimal
decimalFromInteger integer places =
  Decimal {integer = integer * (10 ^ places), places}

-- | Set the decimal precision to larger @p@.
expandDecimalPrecision :: Natural -> Decimal -> Decimal
expandDecimalPrecision new Decimal {integer, places = old} =
  Decimal {places = new, integer = integer * (10 ^ (new - old))}

-- | All naturals have a total conversion to SomeNat.
someNaturalVal :: Natural -> SomeNat
someNaturalVal =
  fromMaybe (error "someNaturalVal: impossible occurred. Report as bug.") .
  someNatVal . fromIntegral

--------------------------------------------------------------------------------
-- Printing

-- Helper type, only needed for showDecimal
data Places (n :: Nat)
instance KnownNat n => HasResolution (Places n) where
  resolution _ = natVal (Proxy @n)

-- > showDecimal Decimal{integer=1250, places = 2}
-- => 12.50
showDecimal :: Decimal -> String
showDecimal Decimal {places, integer} =
  case someNaturalVal (placesToResolution places) of
    SomeNat (_ :: Proxy p) ->
      showFixed False (MkFixed integer :: Fixed (Places p))

--------------------------------------------------------------------------------
-- Helpers

placesToResolution :: Natural -> Natural
placesToResolution n = 10 ^ n
