{-# LANGUAGE NoImplicitPrelude, DataKinds #-}

module MatrixExtrasSpec (tests) where

import Test.Hspec
import PrimeField
import NumericPrelude
import MathObj.Matrix
import MatrixExtras

type GF2 = PrimeField.T 2
type GF5 = PrimeField.T 5

m1 :: MathObj.Matrix.T GF2
m1 = fromRows 3 3 [[e 1, e 0, e 0],
                   [e 0 , e 1, e 0],
                   [e 0, e 0, e 1]]

em1 = echelon m1

m2 :: MathObj.Matrix.T GF2
m2 = fromRows 3 3 [[e 1, e 0, e 1],
                   [e 0, e 1, e 1],
                   [e 1, e 1, e 0]]

em2 = echelon m2

m3 :: MathObj.Matrix.T GF2
m3 = fromRows 3 3 [[e 0, e 0, e 0],
                   [e 0, e 0, e 0],
                   [e 0, e 0, e 0]]

m4 :: MathObj.Matrix.T Float
m4 = fromRows 3 3 [[1.0, 2.0, 3.0],
                   [1.0, 1.0, 1.0],
                   [2.0, 1.0, 1.0]]

c :: MathObj.Matrix.T GF2
c = fromColumns 3 1 [[e 0, e 1, e 0]]

z :: MathObj.Matrix.T GF2
z = fromColumns 1 1 [[e 0]]

m5 :: MathObj.Matrix.T GF2
m5 = fromColumns 2 1 [[e 1, e 1]]

m6 :: MathObj.Matrix.T GF5
m6 = fromRows 3 2 [[e 1, e 1], [e 2, e 2], [e 3, e 3]]

tests = describe "MatrixExtras tests" $ do
  it "Internal: swaps two rows" $ do
    apply (Swap 0 2) m6 `shouldBe` fromRows 3 2 [[e 3, e 3], [e 2, e 2], [e 1, e 1]]

  it "Internal: swaps a row with itself" $ do
    apply (Swap 2 2) m6 `shouldBe` m6

  it "Internal: multiplies one row" $ do
    apply (Mul 1  (e 2)) m6 `shouldBe`
      fromRows 3 2 [[e 1, e 1], [e 4, e 4], [e 3, e 3]]

  it "Internal: multiplies one row and adds it to another" $ do
    apply (Add 0 2 (e 2)) m6 `shouldBe`
      fromRows 3 2 [[e 1, e 1], [e 2, e 2], [e 0, e 0]]

  it "Gaussian reduction" $ do
    gauss m6 `shouldBe` fromRows 3 2 [[e 1, e 1], [e 0, e 0], [e 0, e 0]]

  it "Gaussian reduction of zero" $
    gauss m3 `shouldBe` m3

  it "Reduces a matrix to echelon form" $ do
    (echelon m1) `shouldBe` diagonal [e 1, e 1, e 1]

  it "Computes the rank of a matrix" $ do
    (rank m1) `shouldBe` 3

  it "Computes the determinant of a GF(2) matrix" $ do
    (det m2) `shouldBe` e 0

  it "Computes the determinant of a Float matrix" $ do
    (det m4) `shouldSatisfy` (\x -> x >= -1.01 && x <= -0.99)

  it "Computes the rank of a column matrix" $ do
    (rank c) `shouldBe` 1

  it "Computes the rank of a row matrix" $ do
    (rank $ transpose c) `shouldBe` 1

  it "Computes the rank of a 1 x 1 matrix" $ do
    (rank $ (transpose c) * c) `shouldBe` 1

  it "Computes the rank of null 1 x 1 matrix" $ do
    (rank z) `shouldBe` 0

  it "Computes the rank of [1, 1]t" $ do
    (rank m5) `shouldBe` 1

  it "Computes the rank of [1, 1]" $ do
    (rank $ transpose m5) `shouldBe` 1
