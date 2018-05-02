{-# LANGUAGE NoImplicitPrelude #-}

module MatrixExtras where

import NumericPrelude
import Algebra.Field
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

reorderRows :: (Algebra.Additive.C a, Eq a) => [[a]] -> [[a]]
reorderRows (row0:rows) = if (head row0 /= Algebra.Additive.zero) then (row0:rows)
                          else snd (reorderRowsI row0 rows) :
                               fst (reorderRowsI row0 rows)

mulRow s row = map ((*) s) row
negRow row = map negate row
addRow row0 row1 = map (\(x, y) -> x + y) (zip row0 row1)
subtractRow r [] = []
subtractRow r (row:rows) =
  if first == Algebra.Additive.zero then (row : (subtractRow r rows))
  else ((pivot / first) `mulRow` row) `addRow` (negRow r) : (subtractRow r rows)
  where pivot = head r
        first = head row

dropLeft rows = map tail rows
padLeft rows = map ((:) Algebra.Additive.zero) rows

echelonR :: (Algebra.Field.C a, Eq a) => [[a]] -> [[a]]
echelonR [] = []
echelonR [row] = [row]
echelonR rows = row0 : (padLeft . echelonR . dropLeft) (subtractRow row0 row1s)
  where row0 =  head $ reorderRows rows
        row1s = tail $ reorderRows rows

echelon :: (Algebra.Field.C a, Eq a) => MathObj.Matrix.T a -> MathObj.Matrix.T a
echelon mat = ((fromRows m n) . echelonR . rows) mat
  where m = fst $ dimension mat
        n = snd $ dimension mat
