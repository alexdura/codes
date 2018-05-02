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
import Algebra.ZeroTestable
import MathObj.Polynomial
import Data.Maybe
import Data.Ix
import Data.Foldable
import Data.String
import Data.List

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

instance KnownNat p => Show (PrimeFieldElement p) where
  show (PrimeFieldElement x) = show x

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

instance KnownNat p => Algebra.ZeroTestable.C (PrimeFieldElement p) where
  isZero px@(PrimeFieldElement x) = x `mod` (order px) == 0


pretty :: KnownNat p => MathObj.Polynomial.T (PrimeFieldElement p) -> String -> String
pretty p var =
  if isZero p then "0"
  else foldr1 cat $ map pmono (Data.List.reverse $ filter (not . isZero . snd) $
                                  zip (range (0, degp)) (coeffs p))
  where cat s t = s ++ "+" ++ t
        pmono (n, c) = if n == 0 then (show c)
                       else if n == 1 then if c == one then var
                                           else (show c)  ++ var
                       else if c == one then var ++ "^" ++ (show n)
                       else (show c) ++ var ++ "^" ++ (show n)
        degp = fromMaybe 0 (degree p)


e :: KnownNat p => Integer -> PrimeFieldElement p
e x = PrimeFieldElement x
