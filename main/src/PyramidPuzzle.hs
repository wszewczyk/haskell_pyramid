-----------------------------------------------------------------------------
--
-- Module      :  PyramidPuzzle
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module PyramidPuzzle (Pyramids (Pyramids), solve) where
import Control.Monad
import Data.List
import Data.Map
import Data.Maybe

data Pyramids = Pyramids [Maybe Int] [Maybe Int] [Maybe Int] [Maybe Int]
  deriving (Show, Read)

type PartialBoard = ([[Int]], Data.Map.Map Int [Int])
data Partial = Partial Int PartialBoard (Int, Int) deriving Show

solve :: Pyramids -> Maybe [[Int]]
solve (Pyramids [] [] [] []) = Just []
solve (Pyramids above below left right) =
  listToMaybe (fmap extractSolution (backtrack range done reject apply empty))
  where
    range  = [1..n]
    advance (x, y) = let (dx, x') =  quotRem (x + 1) n in (x', y + dx)
    done (Partial n _ (x, y)) = (x, y) == (0, n)
    reject partial@(Partial n board (x,y)) value =
      elem value row ||
      elem value column ||
      (if y == n-1
        then rejectByEdgeConstraint (below !! x) (value:column) ||
             rejectByEdgeConstraint (above !! x) (reverse  (value:column))
        else False) ||
      (if x == n-1
        then rejectByEdgeConstraint (right !! y) (value:row) ||
             rejectByEdgeConstraint (left !! y) (reverse (value:row))
        else False)

      where row = currentRow partial
            column = currentColumn partial
            rejectByEdgeConstraint e r = not (lineConstraintHolds e r)
    apply p@(Partial n board pos) value = Partial n (addToBoard p value) (advance pos)

    empty = Partial n (emptyBoard n) (0, 0)
    n = length left

backtrack :: [b] -> (a -> Bool) -> (a -> b -> Bool) ->
               (a -> b -> a) -> a -> [a]
backtrack range done reject apply partial = do
  step <- range
  when (reject partial step) []
  let partial' = apply partial step
  if done partial'
    then return partial'
    else backtrack range done reject apply partial'

currentRow :: Partial -> [Int]
currentRow (Partial _ ([], _) _) = []
currentRow (Partial _ _ (0, _)) = []
currentRow (Partial _ (row:_, _) _) = row

currentColumn :: Partial -> [Int]
currentColumn (Partial size (_, columns) (x, _)) = columns Data.Map.! x

addToBoard :: Partial -> Int -> PartialBoard
addToBoard (Partial size (rows, columns) (0, _)) value =
  ([value]:rows, adjust (value:) 0 columns)
addToBoard (Partial size (row:rows, columns) (x, _)) value =
  ((value:row):rows, adjust (value:) x columns)

emptyBoard n = ([], Data.Map.fromList [(i, []) | i <- [0..n-1]])

extractSolution :: Partial -> [[Int]]
extractSolution (Partial size (rows, _) _) = reverse  (fmap reverse  rows)

-- Check if a row or column adheres to a constraint on its
-- beginning, i.e. do I see i pyramids from the point of list's
-- beginning.  Always True if there is no constraint.
lineConstraintHolds :: Maybe Int -> [Int] -> Bool
lineConstraintHolds Nothing _ = True
lineConstraintHolds (Just i) xs = lineConstraintFor xs == i

-- How many pyrmids can be seen from the point of list's beginning.
lineConstraintFor :: [Int] -> Int
lineConstraintFor line = inner 0 0 line
  where inner _ i [] = i
        inner max i (h:t) = if h > max then inner h (i+1) t
                                       else inner max i t
