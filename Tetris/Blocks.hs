module Tetris.Blocks(
  Block(..),
  BlockType(..),
  Color(..),
  Square(..),
  move, rotate, rotateLeft,
  absoluteLocation

) where

import Data.Function (on)

data BlockType = I | L | J | S | Z | O | T
  deriving (Show, Eq)

data Square = Square {
  squareLoc :: Coord,
  squareColor :: Color
} deriving (Show)

type Coord = (Int, Int) 

-- could definitely have a more efficient storage method, but again
-- I'm more interested in 'elegance' for a board of this size

data Color = Red | Orange | Yellow | Green | Blue | Indigo | Violet
  deriving (Show, Eq)

data Block = Block {
  blockType :: BlockType,
  blockLoc :: Coord,
  squares :: [Square]
} deriving (Show)

instance Eq Square where
  (==) a b = ((==) `on` squareLoc) a b

class Shape a where
  rotate :: a -> a
  rotateLeft :: a -> a
  rotateLeft = rotate . rotate . rotate
  move :: Coord -> a -> a

instance Shape Square where
  rotate = id
  move (x,y) s = s { squareLoc = (x + x0, y + y0) }
    where (x0, y0) = squareLoc s

instance Shape Block where
  move (x,y) b = b { blockLoc = (x + x0, y + y0) }
    where (x0, y0) = blockLoc b
  rotate b = b { squares = 
    map (\s -> s { squareLoc = (rotateCoord . squareLoc) s }) $ squares b }
    where
      rotateCoord :: Coord -> Coord
      rotateCoord (x, y) = (-y, x)

absoluteLocation :: Block -> [Square]
absoluteLocation b = 
  let loc = blockLoc b
  in map (move loc) $ squares b


