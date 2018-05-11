{-# LANGUAGE NoImplicitPrelude, ScopedTypeVariables #-}

module RSCode where

import NumericPrelude
import PrimeField
import Algebra.Field
import Algebra.Ring
import MathObj.Polynomial
import Number.ResidueClass
import Number.ResidueClass.Check
import GHC.TypeLits
import Combinatorics
import MathObj.Matrix
import Galois
import Data.Proxy
import NumericPrelude.Numeric

type GF p n = Galois.T p n

generator :: forall p n . KnownNat p => KnownNat n => Int -> MathObj.Matrix.T (GF p n)
generator k  =
  fromRows k (length els) $ map elsPow [0..toInteger(k-1)]
  where
    els = Galois.elements
    elsPow m = map (\x -> x^m) els
    p' = natVal (Proxy :: Proxy p)

check :: forall p n . KnownNat p => KnownNat n => Int -> MathObj.Matrix.T (GF p n)
check k = generator (q - k)
  where q = NumericPrelude.fromInteger $ (Galois.order (Algebra.Ring.one :: ((GF p) n)))
