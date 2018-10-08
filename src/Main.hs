module Main where

import Control.Monad
import Data.Maybe
import qualified Data.Map as M
import Text.Printf

import Types
import Board
import Russian

main :: IO ()
main = do
  let brd = linkBoard [line1, line2, line3, line4, line5, line6, line7, line8]
  forM_ brd $ \line ->
    print $ map showAddress2 line
--   let (l3, l4) = linkLines1 line3 line4
--   print $ map showAddress2 l3
--   print $ map showAddress2 l4
  let a3 = resolve "a3" board8
  let b4 = resolve "b4" board8
  print $ showAddress2 a3
  print $ showAddress $ fromJust $ neighbour (myDirection First ForwardRight) a3
  print $ isLastHorizontal First b4
  let move = simpleMove First a3 ForwardRight
  let ok = isWellFormedMove First board8 move
  print move
  print ok
  let moves = possibleMoves Russian First board8
  print moves

