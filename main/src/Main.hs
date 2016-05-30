-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--

module Main (
    main
) where
import Data.List (last)
import System.Environment (getArgs)
import System.Environment (withArgs)
import Control.Monad
import PyramidPuzzle
main = do
  --let args = ["sss","puzzle"]
  args <- getArgs
  if length args < 1
    then error "Use command: main <input file>"
    else do
      let fileName = last args
      cont <- readFile fileName
      let puzzle@(Pyramids above below left right) = read cont :: Pyramids
          solution = findSolution puzzle
      print solution
