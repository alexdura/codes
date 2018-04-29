{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes, GADTs, TypeOperators, DataKinds, KindSignatures, ScopedTypeVariables, TypeApplications, InstanceSigs #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Galois where

import Data.Proxy
import GHC.TypeLits
import NumericPrelude
import Algebra.Field
import Algebra.Additive
import Algebra.Ring

-- GF(p) instances
data PrimeFieldElement (p :: Nat) where
  PrimeFieldElement :: Integer -> PrimeFieldElement (p :: Nat)
  deriving Eq

order :: forall p . KnownNat p => (PrimeFieldElement p) -> Integer
order _ = n
  where
    n :: Integer
    n = natVal (Proxy :: Proxy p)

data G2 = E0
        | E1
        deriving (Eq)

instance Show (PrimeFieldElement p) where
  show (PrimeFieldElement p) = show p

addInternal :: KnownNat p =>
               (PrimeFieldElement p) -> (PrimeFieldElement p) -> (PrimeFieldElement p)
addInternal px@(PrimeFieldElement x) (PrimeFieldElement y) =
  PrimeFieldElement ((x + y) `mod` (order px))

negateInternal :: KnownNat p =>
                  (PrimeFieldElement p) -> (PrimeFieldElement p)
negateInternal px@(PrimeFieldElement x) = PrimeFieldElement ((order px) - x)

instance KnownNat p => Algebra.Additive.C (PrimeFieldElement p) where
  zero = (PrimeFieldElement 0)
  (+) = addInternal
  negate = negateInternal

instance KnownNat p => Algebra.Ring.C (PrimeFieldElement p) where
  one = (PrimeFieldElement 1)
  (*) px@(PrimeFieldElement x) (PrimeFieldElement y) = PrimeFieldElement ((x * y) `mod` (order px))

instance KnownNat p => Algebra.Field.C (PrimeFieldElement p) where
  recip x = x ^ ((order x) - 1)


-- GF(2) instance
instance Show G2 where
  show E0 = "0"
  show E1 = "1"

instance Algebra.Additive.C G2 where
  zero = E0
  (+) e0 e1 = if e0 == E0 then e1
              else if e1 == E0 then e0
                   else E0
  negate = id

instance Algebra.Ring.C G2 where
  one = E1
  (*) e0 e1 = if e0 == E0 then E0
              else e1

instance Algebra.Field.C G2 where
  recip = id
