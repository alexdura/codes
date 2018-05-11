{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes, GADTs, TypeOperators, DataKinds, KindSignatures, ScopedTypeVariables, TypeApplications, InstanceSigs  #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Galois where

import NumericPrelude
import PrimeField
import MathObj.Polynomial
import GHC.TypeLits
import Data.Maybe
import Data.Proxy
import Algebra.Additive
import Algebra.Ring
import Algebra.Field
import Algebra.ZeroTestable

polynomials :: KnownNat p => Int -> [MathObj.Polynomial.T (PrimeField.T p)]
polynomials n = map fromCoeffs $ map (\x -> x ++ [Algebra.Ring.one]) coeffs
                where coeffs = sequence $ replicate n PrimeField.elements

-- a recursive definition of all irreducible monic polynomials of degree <= n
-- (and a totally naive implementation)
irreducible :: KnownNat p => Int -> [MathObj.Polynomial.T (PrimeField.T p)]
irreducible 1 = polynomials 1
irreducible n =
  [p | p <- (polynomials n),
   all (\q -> (not . isZero) (p `mod` q)) (irreducible (n `div` 2))]
  ++ (irreducible (n - 1))

data T p n where
  Cons :: (MathObj.Polynomial.T (PrimeField.T p)) ->
          (MathObj.Polynomial.T (PrimeField.T p)) -> Galois.T (p :: Nat) (n :: Nat)

representative (Galois.Cons _ p) = p

degree :: forall n . (KnownNat n) => forall p . (KnownNat p) => Proxy (Galois.T p n) -> Integer
degree _ = natVal (Proxy :: Proxy n)

degree' :: forall n . (KnownNat n) => forall p . (KnownNat p) => (Galois.T p n) -> Integer
degree' _ = n'
  where n' = natVal (Proxy :: Proxy n)

order' :: forall p . (KnownNat p) => forall n . (KnownNat n) => (Galois.T p n) -> Integer
order' _ = p'
  where p' = natVal (Proxy :: Proxy p)

order :: forall p . (KnownNat p) => forall n . (KnownNat n) => (Galois.T p n) -> Integer
order x = (order' x) ^ (degree' x)
---------------------------------------------------------
-- Smart constructor
fromPolynomial :: forall n . (KnownNat n) => forall p . (KnownNat p) =>
                  MathObj.Polynomial.T (PrimeField.T p) -> (Galois.T p n)

fromPolynomial pol = Galois.Cons m (pol `mod` m)
  where n' = natVal (Proxy :: Proxy n)
        m = head $ irreducible (NumericPrelude.fromInteger n')

modulus :: forall n . (KnownNat n) => forall p . (KnownNat p) => Proxy (Galois.T p n) -> MathObj.Polynomial.T (PrimeField.T p)
modulus _ = head $ irreducible (NumericPrelude.fromInteger (natVal (Proxy :: Proxy n)))

-- Instances
instance (KnownNat p, KnownNat n) => Algebra.Additive.C (Galois.T p n) where
  zero = fromPolynomial Algebra.Additive.zero
  (+) (Galois.Cons _ p) (Galois.Cons _ q) = fromPolynomial (p + q)
  negate (Galois.Cons _ p) = fromPolynomial (negate p)

instance (KnownNat p, KnownNat n) => Algebra.Ring.C (Galois.T p n) where
  one = fromPolynomial Algebra.Ring.one
  (*) (Galois.Cons _ p) (Galois.Cons r q) = fromPolynomial ((p * q) `mod` r)

instance (KnownNat p, KnownNat n) => Algebra.Field.C (Galois.T p n) where
  recip x = x ^ ((Galois.order x) - 1)

instance (KnownNat p, KnownNat n) => Show (Galois.T p n) where
  show (Cons m p) = "[" ++ show p ++ "/" ++ show m ++ "]"

instance (KnownNat p, KnownNat n) => Eq (Galois.T p n) where
  (Cons _ p) == (Cons _ q) = p == q

instance (KnownNat p, KnownNat n) => Algebra.ZeroTestable.C (Galois.T p n) where
  isZero (Cons _ p) = isZero p

pretty :: (KnownNat p, KnownNat n) => (Galois.T p n) -> String -> String

pretty p var = PrimeField.pretty (Galois.representative p) var

elements :: forall n . KnownNat n => forall p . KnownNat p => [Galois.T p n]
elements =
  map (fromPolynomial . fromCoeffs) (sequence $ replicate deg PrimeField.elements)
  where deg = (NumericPrelude.fromInteger $ (natVal (Proxy :: Proxy n))) :: Int
