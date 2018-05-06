{-# LANGUAGE NoImplicitPrelude #-}

module LinearCode where

import NumericPrelude
import Algebra.ZeroTestable
import PrimeField
import MatrixExtras
import GHC.TypeLits
import MathObj.Matrix
import Combinatorics
import Algebra.Field

weight :: Algebra.ZeroTestable.C a => [a] -> Int
weight [] = 0
weight (x:xs) = if isZero x then weight xs
                else 1 + weight xs

-- generate all vectors (lists) of n elements in GF(p)
vectorsOfN :: KnownNat p => Int -> [[PrimeField.T p]]
vectorsOfN n = sequence $ replicate n elements

hasStrength :: (Eq a , Algebra.Field.C a) => Int -> MathObj.Matrix.T a -> Bool
hasStrength t mat =
  all (\cs -> rank (fromColumns m t cs) == t) (tuples t (columns mat))
  where (m, n) = dimension mat

strength :: KnownNat p => Dimension -> Dimension -> Int  -> [MathObj.Matrix.T (PrimeField.T p)]
strength m n t =
  [fromColumns m n cols | cols <- tuples n vectors, hasStrength t (fromColumns m n cols)] where
    vectors = vectorsOfN m

check :: KnownNat p => Int -> Int -> Int -> MathObj.Matrix.T (PrimeField.T p)
check n k d = head $ strength (n - k) n (d - 1)

generator :: KnownNat p => Int -> Int -> Int -> MathObj.Matrix.T (PrimeField.T p)
generator n k d = head gens where
  vectors = filter (\v -> weight v >= d) (vectorsOfN n)
  gens = [fromRows k n rows | rows <- tuples k vectors, rank (fromRows k n rows) == k]
