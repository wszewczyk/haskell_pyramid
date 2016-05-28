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

import Control.Monad
import Data.List (last)
import System.Environment (getArgs)
import System.Environment (withArgs)

import PyramidPuzzle



main = do
  let args = ["sss","case1"]
  --args <- getArgs
  if length args < 1
    then error "usage: piramidy <input file>"
    else do
      let fileName = last args
          prettyPrinting = "--pretty" `elem` args

      description <- readFile fileName
      let problem@(Pyramids above below left right) = read description :: Pyramids
          solution = solve problem

          prettyConstr (Nothing) = " "
          prettyConstr (Just i) = show i

      if prettyPrinting
      then do
        case solution of
          Nothing -> putStrLn "no solution"
          Just rows -> do
            putStr " "
            forM_ above (putStr . prettyConstr)
            putStrLn ""

            forM_ (zip3 left rows right) $ \(l, row, r) -> do
                putStr $ prettyConstr l
                forM_ row (putStr . show)
                putStrLn $ prettyConstr r

            putStr " "
            forM_ below (putStr . prettyConstr)
            putStrLn ""
      else
        print solution


--var = "s"
----range = [1 .. 3]
--p 41 range = do
--  step <- range
--  show (step +1)
--  p 42 range
--p 42 range = do
--  step <- range
--  show (step +2)
--p i range= do
--  let j = i + 1
--  p j range
test = [1,2,3] !! 1
