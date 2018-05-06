{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes, GADTs, TypeOperators, DataKinds, KindSignatures, ScopedTypeVariables, TypeApplications, InstanceSigs #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module PrimeField where

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
data T (p :: Nat) where
  T :: Integer -> PrimeField.T (p :: Nat)
  deriving Eq

order :: forall p . KnownNat p => (PrimeField.T p) -> Integer
order _ = n
  where
    n :: Integer
    n = natVal (Proxy :: Proxy p)

data G2 = E0
        | E1
        deriving (Eq)

instance KnownNat p => Show (PrimeField.T p) where
  show (PrimeField.T x) = show x

addInternal :: KnownNat p =>
               (PrimeField.T p) -> (PrimeField.T p) -> (PrimeField.T p)
addInternal px@(PrimeField.T x) (PrimeField.T y) =
  PrimeField.T ((x + y) `mod` (order px))

negateInternal :: KnownNat p =>
                  (PrimeField.T p) -> (PrimeField.T p)
negateInternal px@(PrimeField.T x) = PrimeField.T ((order px) - x)

instance KnownNat p => Algebra.Additive.C (PrimeField.T p) where
  zero = (PrimeField.T 0)
  (+) = addInternal
  negate = negateInternal

instance KnownNat p => Algebra.Ring.C (PrimeField.T p) where
  one = (PrimeField.T 1)
  (*) px@(PrimeField.T x) (PrimeField.T y) = PrimeField.T ((x * y) `mod` (order px))

instance KnownNat p => Algebra.Field.C (PrimeField.T p) where
  recip x = x ^ ((order x) - 1)

instance KnownNat p => Algebra.ZeroTestable.C (PrimeField.T p) where
  isZero px@(PrimeField.T x) = x `mod` (order px) == 0


pretty :: KnownNat p => MathObj.Polynomial.T (PrimeField.T p) -> String -> String
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


e :: KnownNat p => Integer -> PrimeField.T p
e x = PrimeField.T x
