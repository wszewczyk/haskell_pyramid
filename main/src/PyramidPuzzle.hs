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

module PyramidPuzzle (Pyramids (Pyramids), findSolution) where
import Control.Monad
import Data.List
import Data.Map
import Data.Maybe
import Data.Matrix
import Data.Vector

data Pyramids = Pyramids [Maybe Int] [Maybe Int] [Maybe Int] [Maybe Int]
  deriving (Show,Read)

type Board = Matrix Int
type BoardRow = Vector Int
type BardCol = Vector Int
type Position = (Int, Int)

data BoardManager = BoardManager Int Board Position

findSolution :: Pyramids -> Maybe [[Int]]
findSolution (Pyramids [] [] [] []) = Just []
findSolution (Pyramids above below left right) =
  listToMaybe (fmap retMatrix (lookForSolution range done reject apply manager))
  where
    step (r, c) = let (dc, rc) =  quotRem (c+1) (size+1) in (r + dc,dc + rc)
    done (BoardManager n _ (r, c)) = (r, c) == (n, n)
    reject manager@(BoardManager n board (r,c)) value =
      Data.List.elem value nrow    --checks whether a row includes passed value
      ||
      Data.List.elem value ncol    --checks whether a column includes passed value
      ||
      if r == n  -- invoked when matrix is created, checks whether the conditions are met
        then isCorrect (below !! (c-1)) (Data.List.reverse ccol Data.List.++ [value]  )||
             isCorrect (above !! (c-1))  (ccol Data.List.++ [value])
        else False
      ||
      if c == n  -- invoked when matrix is created, checks whether the conditions are met
        then isCorrect (right !! (r-1)) (Data.List.reverse crow Data.List.++ [value] )||
             isCorrect (left !! (r-1)) (crow Data.List.++ [value])
        else False
      where crow = currentRow manager --row of a last modufied cell
            ccol = currentColumn manager --column of a last modufied cell
            nrow = nextRow manager --row of a next cell (where will be inserted value)
            ncol = nextCol manager --column of a next cell (where will be inserted value)
            isCorrect e r = not (isGood e r)
    apply p@(BoardManager n board pos) value = BoardManager n (addToBoard p value) (step pos)

    manager = BoardManager size (emptyBoard size) (1, 0)
    range  = [1..size]
    size = Data.List.length left

lookForSolution :: [b] -> (a -> Bool) -> (a -> b -> Bool) ->
               (a -> b -> a) -> a -> [a]
lookForSolution range done reject apply manager = do
  val <- range
  when (reject manager val) []
  let _manager = apply manager val
  if done _manager
    then return _manager
    else lookForSolution range done reject apply _manager

currentRow :: BoardManager -> [Int]
currentRow (BoardManager _ m (r, _)) = Data.Vector.toList (Data.Matrix.getRow r m)
currentColumn :: BoardManager -> [Int]
currentColumn (BoardManager _ m (_, c)) =if (c/=0) then Data.Vector.toList (Data.Matrix.getCol c m)
                                              else Data.Vector.toList (Data.Matrix.getCol 1 m)

nextRow :: BoardManager -> [Int]
nextRow (BoardManager size m (r, c)) = let (nr,nc) = (step (r,c)) in
                                    Data.Vector.toList (Data.Matrix.getRow nr m)
                                    where step (r, c) = let (dc, rc) =  quotRem (c+1) (size+1) in (r + dc,dc + rc)
nextCol :: BoardManager -> [Int]
nextCol (BoardManager size m (r, c)) = let (nr,nc) = (step (r,c)) in
                                    Data.Vector.toList (Data.Matrix.getCol nc m)
                                    where step (r, c) = let (dc, rc) =  quotRem (c+1) (size+1) in (r + dc,dc + rc)

addToBoard :: BoardManager -> Int -> Board
addToBoard (BoardManager size board (r, c)) value =
  Data.Matrix.setElem value (ad (r,c)) board where
  ad (r, c) = let (dc, c') =  quotRem (c+1) (size+1) in (r + dc,dc + c')
emptyBoard n = (Data.Matrix.zero n n)
isGood :: Maybe Int -> [Int] -> Bool
isGood Nothing _ = True
isGood (Just i) xs = numOfVisible xs == i

numOfVisible :: [Int] -> Int
numOfVisible line = _numOfVisible 0 0 line
  where _numOfVisible _ i [] = i
        _numOfVisible max i (x:xs) = if x > max then _numOfVisible x (i+1) xs
                                       else _numOfVisible max i xs
retMatrix :: BoardManager -> [[Int]]
retMatrix (BoardManager size b _) = Data.Matrix.toLists b  --solution as array of arrays


