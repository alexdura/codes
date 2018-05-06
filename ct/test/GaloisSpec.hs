{-# LANGUAGE NoImplicitPrelude, DataKinds #-}

module GaloisSpec (tests) where

import Test.Hspec
import PrimeField
import Galois
import NumericPrelude
import MathObj.Matrix

-- 0
e0 = gf4 $ (map e) [0, 0]
-- 1
e1 = gf4 $ (map e) [1, 0]
-- X
e2 = gf4 $ (map e) [0, 1]
-- X+1
e3 = e1 + e2

tests = describe "Galois Field tests" $ do
  it "Computes X*(X+1) in GF(4)" $ do
    e2 * e3 `shouldBe` e1

  it "Computes (X+1)^2 in GF(4)" $ do
    e3 * e3 `shouldBe` e2

  it "Computes X^2 in GF(4)" $ do
    e2 * e2 `shouldBe` e3
