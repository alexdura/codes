{-# LANGUAGE NoImplicitPrelude, DataKinds #-}

module MatrixExtrasSpec (tests) where

import Test.Hspec
import Galois
import NumericPrelude
import MathObj.Matrix
import MatrixExtras

type GF2 = Galois.PrimeFieldElement 2


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

tests = describe "MatrixExtras tests" $ do
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
