module Tetris.Board(
  clearLines,
  Board,
  dropLines
) where

import Tetris.Blocks
import Data.Function (on)
import Data.List (sortBy, groupBy)

type Board = [Square]


clearLines :: Int -> Board -> Board
clearLines width board = 
  concat . clearFull . groupList . sortList $ board
  where
    yVec = snd . squareLoc
    sortList = sortBy (compare `on` yVec)
    groupList = groupBy ((==) `on` yVec)
    clearFull = filter ((/=) width . length)

dropLines :: Board -> Board
dropLines = id

