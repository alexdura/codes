{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes, GADTs, TypeOperators, DataKinds, KindSignatures, ScopedTypeVariables, TypeApplications, InstanceSigs #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Galois where

import NumericPrelude
import PrimeField
import MathObj.Polynomial
import Number.ResidueClass.Check
import GHC.TypeLits

type GF2 = PrimeField.T 2
type P2 = MathObj.Polynomial.T GF2

gf4 :: [GF2] -> Number.ResidueClass.Check.T P2
gf4 x = Number.ResidueClass.Check.Cons (fromCoeffs [e 1, e 1, e 1]) (fromCoeffs x)

pretty :: KnownNat p => Number.ResidueClass.Check.T (MathObj.Polynomial.T (PrimeField.T p)) -> String ->String

pretty p var = PrimeField.pretty (representative p) var
