{-# LANGUAGE NoImplicitPrelude, DataKinds #-}

module RSCodeSpec (tests) where

import Test.Hspec
import PrimeField
import Galois
import NumericPrelude
import MathObj.Matrix
import RSCode
import MatrixExtras
import MathObj.Polynomial
import Algebra.ZeroTestable

g2 = generator 2 :: MathObj.Matrix.T (Galois.T 2 2)
c2 = check 2  :: MathObj.Matrix.T (Galois.T 2 2)
type P p = MathObj.Polynomial.T (PrimeField.T p)

isAllZeros m = all isZero $ (concat . columns) m

tests = describe "RS codes tests" $ do
  it "Lists all irreducile polynomials of degree 1 over GF(3)" $ do
    (irreducible 1 :: [P 3]) `shouldBe` [
      fromCoeffs $ map e [0,1],
      fromCoeffs $ map e [1,1],
      fromCoeffs $ map e [2,1]]

  it "Verifies that check * generator is zero" $ do
    c2 * (transpose g2) `shouldSatisfy` isAllZeros
