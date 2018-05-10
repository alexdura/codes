{-# LANGUAGE NoImplicitPrelude, DataKinds #-}

module LinearCodeSpec (tests) where

import Test.Hspec
import PrimeField
import Galois
import NumericPrelude
import MathObj.Matrix
import LinearCode
import MatrixExtras

s1 :: [MathObj.Matrix.T (PrimeField.T 2)]
s1 = strength 3 7 2

g2 :: MathObj.Matrix.T (PrimeField.T 2)
c2 :: MathObj.Matrix.T (PrimeField.T 2)
c2 = check 7 4 3
g2 = generator 7 4 3

m1 :: MathObj.Matrix.T (PrimeField.T 2)

m1 = fromColumns 2 3 [[e 1, e 0], [e 0, e 1], [e 1, e 1]]

tests = describe "Linear codes tests" $ do
  it "Compute all matrices of strenght t" $ do
    length s1 `shouldBe` 1

  it "Verifies that I2 has strength 2" $ do
    hasStrength 2 m1 `shouldBe` True

  it "Verifies that I2 also has strength 1" $ do
    hasStrength 1 m1 `shouldBe` True

  it "Computes a generator for a Hamming (7, 4, 3) code" $ do
    (gauss g2) `shouldBe` MathObj.Matrix.zero 4 4
