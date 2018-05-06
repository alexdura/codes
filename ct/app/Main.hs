{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes, GADTs, TypeOperators, DataKinds, KindSignatures, ScopedTypeVariables, TypeApplications, InstanceSigs #-}
--{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Main where

--import Galois
import PrimeField
import Lib
import System.IO

import NumericPrelude

import Algebra.Additive ((+))
import Algebra.Ring ((*))
import Algebra.IntegralDomain (mod)
import MathObj.Polynomial
import MathObj.Matrix

import Data.String
import Data.List
import Prelude(show)

import MatrixExtras

import GHC.Stack

import LinearCode

type GF5 = PrimeField.T 5
type GF2 = PrimeField.T 2
type GF3 = PrimeField.T 3


s3 :: GF5
s3 = PrimeField.T 4
s2 :: GF5
s2 = PrimeField.T 1
r :: GF5
r = s3 + s2
s = s2 * s2

p1 :: MathObj.Polynomial.T GF5
p1 = fromCoeffs [PrimeField.T 1,
                 PrimeField.T 2,
                 PrimeField.T 3]

p2 :: MathObj.Polynomial.T GF5
p2 = fromCoeffs [PrimeField.T 1,
                 PrimeField.T 2]

p3 = p1 `mod` p2
p4 = p1 * p2

m1 :: MathObj.Matrix.T GF2
m1 = fromRows 3 3 [[e 1, e 0, e 0], [e 0 , e 1, e 0], [e 0, e 0, e 1]]

em1 = echelon m1

m2 :: MathObj.Matrix.T GF2
m2 = fromRows 3 3 [[e 1, e 0, e 1], [e 0, e 1, e 1], [e 1, e 1, e 0]]

em2 = echelon m2

m3 :: MathObj.Matrix.T GF2
m3 = fromRows 3 3 [[e 0, e 0, e 0], [e 0, e 0, e 0], [e 0, e 0, e 0]]

m4 :: MathObj.Matrix.T Float
m4 = fromRows 3 3 [[1.0, 2.0, 3.0], [1.0, 1.0, 1.0], [2.0, 1.0, 1.0]]

c :: MathObj.Matrix.T GF2
c = fromColumns 3 1 [[e 0, e 1, e 0]]

g :: MathObj.Matrix.T GF3
g = generator 4 3 2

main :: IO ()
main = do {
  putStrLn ((show r) ++ " " ++ (show s));
  putStrLn ((show p1));
  putStrLn ((show p2));
  putStrLn ((show p4));
  putStrLn ( pretty p4 "X");

  putStrLn (format m1);
  putStrLn (format em1);
  putStrLn (show $ det $ m1);

  putStrLn (format m2);
  putStrLn (format em2);
  putStrLn (show $ det $ m2);

  putStrLn (show (rank m2));
  putStrLn (show (rank m1));

  putStrLn (format $ echelon m3);
  putStrLn (show $ rank m3);
  putStrLn (show $ det m3);

  putStrLn (format $ m4);
  putStrLn (format $ echelon $ m4);
  putStrLn (show $ rank m4);
  putStrLn (show $ det m4);

  s <- whoCreated $ rank c;
  putStrLn $ renderStack s;

  putStrLn $ (format g);
  }
