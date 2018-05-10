{-# LANGUAGE NoImplicitPrelude #-}

module MatrixExtras where

import NumericPrelude
import Algebra.Field
import Algebra.Ring
import Algebra.Additive
import MathObj.Matrix
import Algebra.ZeroTestable
import Data.List
import Data.Maybe

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
reorderRows ([]:rows) = ([]:rows, False)
reorderRows [row] = ([row], False)
reorderRows (row0:rows) = if (head row0 /= Algebra.Additive.zero)
                          then (row0:rows, False)
                          else (snd (reorderRowsI row0 rows) :
                                fst (reorderRowsI row0 rows), True)

-- Multiply a row with a scalar
mulRow s row = map ((*) s) row
negRow row = map negate row
addRow row0 row1 = map (\(x, y) -> x + y) (zip row0 row1)
-- Subtract a row from a list of rows
subtractRow _ [] = ([], Algebra.Ring.one)
subtractRow [] r = (r, Algebra.Ring.one)
subtractRow _ ([]:rows) = ([]:rows, Algebra.Ring.one)
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
-- echelonR r@([_]:_) =
--   (fst rr, (if snd rr then negate else id) Algebra.Ring.one)
--   where rr = reorderRows r
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

isAllZero :: (Algebra.Field.C a, Algebra.ZeroTestable.C a) => [a] -> Bool
isAllZero [] = True
isAllZero (x:xs) = (isZero x) && (isAllZero xs)

rank :: (Algebra.Field.C a, Algebra.ZeroTestable.C a) => MathObj.Matrix.T a -> Int
rank mat = length $ filter (not . isAllZero) (rows $ gauss mat)


data Action a = Swap Int Int -- swap two rows
              | Add Int Int a -- add first row x a to the second row
              | Mul Int a -- multiply row with a
              | MoveRight -- move from position (i, j) to (i, j+1)
              deriving (Eq, Show)

trySwap :: (Algebra.Field.C a, Algebra.ZeroTestable.C a) => Dimension -> Dimension -> MathObj.Matrix.T a -> [Action a]
trySwap i j mat =
  let
    (m, n) = dimension mat
    nonZeroIdx = fromMaybe m $ find (\k -> (not . isZero) (index mat k j)) [i..m-1]
  in
   if nonZeroIdx == i then tryScale i j mat
   else if nonZeroIdx == m then [MoveRight]
        else [Swap i nonZeroIdx]


tryScale :: (Algebra.Field.C a, Algebra.ZeroTestable.C a) => Dimension -> Dimension -> MathObj.Matrix.T a -> [Action a]
tryScale i j mat =
  if (not . isZero) ((index mat i j) - Algebra.Ring.one)
  then [Mul i (recip $ index mat i j)]
  else tryAdd i j mat

tryAdd :: (Algebra.Field.C a, Algebra.ZeroTestable.C a) => Dimension -> Dimension -> MathObj.Matrix.T a -> [Action a]
tryAdd i j mat =
  concat $ map action [k | k <- [0..(m-1)], k /= i]
  where
    (m, n) = dimension mat
    action k = if (not . isZero) (index mat k j)
               then [Add i k (Algebra.Additive.negate $ index mat k j)]
               else []

replaceAt :: Int -> a -> [a] -> [a]
replaceAt _ _ [] = []
replaceAt 0 y (x:xs) = (y:xs)
replaceAt n y (x:xs) = (x : (replaceAt (n-1) y xs))

apply :: Algebra.Field.C a => Action a -> MathObj.Matrix.T a -> MathObj.Matrix.T a
apply (Swap i j) mat = ((fromRows m n) . (replaceAt i cj) . (replaceAt j ci)) r
  where
    (m, n) = dimension mat
    r = rows mat
    ci = r !! i
    cj = r !! j

apply (Add i j x) mat = (fromRows m n) . (replaceAt j new_cj) . rows $ mat
  where
    (m, n) = dimension mat
    cj = (rows mat) !! j
    ci = (rows mat) !! i
    new_cj = map (\(a, b) -> (a * x + b)) (zip ci cj)

apply (Mul i x) mat = (fromRows m n) . (replaceAt i ci) . rows $ mat
  where
    (m, n) = dimension mat
    ci = map ((*) x) ((rows mat) !! i)

gaussI :: (Algebra.Field.C a, Algebra.ZeroTestable.C a) => Dimension -> Dimension -> MathObj.Matrix.T a -> ([Action a], MathObj.Matrix.T a)
gaussI i j mat =
  let (m, n) = dimension mat
      actions = trySwap i j mat
      tmat = foldl (\m -> \a -> apply a m) mat actions
  in
   if (i >= m || j >= n) then ([], mat)
   else case actions of
         [MoveRight] -> gaussI i (j + 1) mat
         [] -> gaussI (i + 1) (j + 1) mat
         otherwise -> (actions ++ fst g, snd g)
           where g = gaussI i j tmat

gauss :: (Algebra.Field.C a, Algebra.ZeroTestable.C a) => MathObj.Matrix.T a -> MathObj.Matrix.T a
gauss mat = snd $ gaussI 0 0 mat

detI :: (Algebra.Field.C a, Algebra.ZeroTestable.C a) => [Action a] -> a
detI [] = Algebra.Ring.one
detI (a:as) = case a of
  (Mul _ x) -> (recip x) * (detI as)
  (Add _ _ x) -> --(recip x) *
    (detI as)
  (Swap _ _) -> (negate Algebra.Ring.one) * (detI as)


diagI :: Dimension -> Dimension -> MathObj.Matrix.T a  -> [a]
diagI i j mat =
  if (i >= m || j >= n) then []
  else (index mat i j):(diagI (i + 1) (j + 1) mat)
  where
    (m, n) = dimension mat

diag :: MathObj.Matrix.T a -> [a]
diag = diagI 0 0

det :: (Algebra.Field.C a, Algebra.ZeroTestable.C a) => MathObj.Matrix.T a -> a
det mat = if m /= n then Algebra.Additive.zero
          else (detI actions) * (foldl (*) Algebra.Ring.one  (diag g))
  where m = fst $ dimension mat
        n = snd $ dimension mat
        (actions, g) = gaussI 0 0 mat
