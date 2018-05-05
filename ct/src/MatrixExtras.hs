{-# LANGUAGE NoImplicitPrelude #-}

module MatrixExtras where

import NumericPrelude
import Algebra.Field
import Algebra.Ring
import Algebra.Additive
import MathObj.Matrix
import Data.List

reorderRowsI :: (Algebra.Additive.C a, Eq a) => [a] -> [[a]] -> ([[a]], [a])
reorderRowsI repl [row0] = if (head row0 /= Algebra.Additive.zero) then ([repl], row0)
                           else ([row0], repl)

reorderRowsI repl (row0:rows) =
  if (head row0 /= Algebra.Additive.zero) then (repl:rows, row0)
  else (row0:(fst (reorderRowsI repl rows)),
        snd (reorderRowsI repl rows))

-- Reorder rows such that the first row does not have zero as the first
-- element. Return true if any rows were swapped.
reorderRows :: (Algebra.Additive.C a, Eq a) => [[a]] -> ([[a]], Bool)
reorderRows [] = ([], False)
reorderRows (row0:rows) = if (head row0 /= Algebra.Additive.zero)
                          then (row0:rows, False)
                          else (snd (reorderRowsI row0 rows) :
                                fst (reorderRowsI row0 rows), True)

-- Multiply a row with a scalar
mulRow s row = map ((*) s) row
negRow row = map negate row
addRow row0 row1 = map (\(x, y) -> x + y) (zip row0 row1)
-- Subtract a row from a list of rows
subtractRow r [] = ([], Algebra.Ring.one)
subtractRow r (row:rows) =
  if pivot == Algebra.Additive.zero then ((row:rows), pivot)
  else if first == Algebra.Additive.zero
       then (row : (fst rest), Algebra.Ring.one)
       else (((pivot / first) `mulRow` row) `addRow` (negRow r) : (fst rest),
        first / pivot * (snd rest))
  where pivot = head r
        first = head row
        rest = subtractRow r rows

dropLeft rows = map tail rows
padLeft rows = map ((:) Algebra.Additive.zero) rows

echelonR :: (Algebra.Field.C a, Eq a) => [[a]] -> ([[a]], a)
echelonR [] = ([], Algebra.Ring.one)
echelonR [row] = ([row], Algebra.Ring.one)
echelonR rows =
  (row0 : (padLeft . fst . echelonR . dropLeft) (fst $ subtractRow row0 row1s),
   ((if neg then id else negate) Algebra.Ring.one) *
   (snd . echelonR . dropLeft . fst) (subtractRow row0 row1s) *
   (snd $ subtractRow row0 row1s))
  where rrows = reorderRows rows
        row0 =  head $ fst $ rrows
        row1s = tail $ fst $ rrows
        neg = snd $ rrows

echelon :: (Algebra.Field.C a, Eq a) => MathObj.Matrix.T a -> MathObj.Matrix.T a
echelon mat = ((fromRows m n) . fst . echelonR . rows) mat
  where m = fst $ dimension mat
        n = snd $ dimension mat

isAllZero :: (Algebra.Field.C a, Eq a) => [a] -> Bool
isAllZero [] = True
isAllZero (x:xs) = (x == Algebra.Additive.zero) && (isAllZero xs)

rank :: (Algebra.Field.C a, Eq a) => MathObj.Matrix.T a -> Int
rank mat = length $ filter (not . isAllZero) (rows $ echelon mat)

diagI [] = []
diagI (row:rows) = (head row : (diagI . dropLeft) rows)

diag :: MathObj.Matrix.T a -> [a]
diag = diagI . rows

det :: (Algebra.Field.C a, Eq a) => MathObj.Matrix.T a -> a
det mat = if m /= n then Algebra.Additive.zero
          else (snd ech) * (foldl (*) Algebra.Ring.one  (diagI $ fst ech))
  where m = fst $ dimension mat
        n = snd $ dimension mat
        ech = echelonR $ rows mat
