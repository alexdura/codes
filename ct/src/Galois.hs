{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes, GADTs, TypeOperators, DataKinds, KindSignatures, ScopedTypeVariables, TypeApplications, InstanceSigs #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Galois where

import NumericPrelude
import PrimeField
import MathObj.Polynomial
import Number.ResidueClass.Check
import GHC.TypeLits
import Data.Maybe

pretty :: KnownNat p => Number.ResidueClass.Check.T (MathObj.Polynomial.T (PrimeField.T p)) -> String -> String

pretty p var = PrimeField.pretty (representative p) var

gf :: KnownNat p => (MathObj.Polynomial.T (PrimeField.T p)) ->
      (MathObj.Polynomial.T (PrimeField.T p)) ->
      (Number.ResidueClass.Check.T (MathObj.Polynomial.T (PrimeField.T p)))

gf mod p = Number.ResidueClass.Check.Cons mod p

type GF2 = PrimeField.T 2
type P2 = MathObj.Polynomial.T GF2
gf4 :: [GF2] -> Number.ResidueClass.Check.T P2
gf4 = (gf $ fromCoeffs [e 1, e 1, e 1]) . fromCoeffs

elements :: KnownNat p => MathObj.Polynomial.T (PrimeField.T p) -> [Number.ResidueClass.Check.T (MathObj.Polynomial.T (PrimeField.T p))]

elements mod = map ((gf mod) . fromCoeffs) (sequence $ replicate (fromMaybe 0 (degree mod) - 1) PrimeField.elements)
