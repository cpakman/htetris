
module Main where

import System.Random
import System.Random.Shuffle (shuffle')
import Control.Monad.State
import Data.Function (on)
import Data.List (intersect, sortBy, groupBy)

data BlockType = I | L | J | S | Z | O | T
  deriving (Show, Eq)

data Square = Square {
  squareLoc :: Coord,
  squareColor :: Color
} deriving (Show)

type Coord = (Int, Int) 

-- could definitely have a more efficient storage method, but again
-- I'm more interested in 'elegance' for a board of this size
type Board = [Square]

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

-- overall game state
data World = World {
  currentBlock :: Block,
  setSquares :: Board,
  futureBlocks :: [Block],
  currentScore :: Int
}

data Move = MDown | MLeft | MRight | Rotate Direction
data Direction = CW | CCW

moveBlock :: Move -> Block -> Block
moveBlock MDown block = move (0,-1) block
moveBlock MLeft block = move (-1,0) block
moveBlock MRight block = move (1,0) block
moveBlock (Rotate CW) block = rotate block
moveBlock (Rotate CCW) block = rotateLeft block

clearLines :: Int -> Board -> Board
clearLines width board = 
  concat . clearFull . groupList . sortList $ board
  where
    yVec = snd . squareLoc
    sortList = sortBy (compare `on` yVec)
    groupList = groupBy ((==) `on` yVec)
    clearFull = filter ((/=) width . length)


type WorldState m = StateT World m

makeMove :: Monad m => Move -> WorldState m Bool
makeMove move = do
  s <- get
  return True

-- certainly not the most efficient way to do this, but with the limited board
-- size, it's not a huge deal... but it looks nicer
validState :: Block -> Board -> Bool
validState block board = null $ intersect board $ absoluteLocation block

-- tetris does not completely randomly pick tiles... It randomizes 2 of each tile
-- then repeats this process
blockList :: RandomGen gen => gen -> [Block]
blockList gen = map genBlock $ blockTypeList gen
  where
    blockCycle :: [BlockType]
    blockCycle = [I,I,L,L,J,J,S,S,Z,Z,O,O,T,T]

    blockTypeList :: RandomGen gen => gen -> [BlockType]
    blockTypeList gen = 
      (shuffle' blockCycle (length blockCycle) gen1) ++ (blockTypeList gen2)

    (gen1, gen2) = split gen

genBlock :: BlockType -> Block
genBlock I = Block I (0,0) [
  Square (0,-1) Red,
  Square (0,0) Red,
  Square (0,1) Red,
  Square (0,2) Red]
genBlock L = Block L (0,0) [
  Square (0,-1) Orange,
  Square (0,0) Orange,
  Square (0,1) Orange,
  Square (1,1) Orange]
genBlock J = Block J (0,0) [
  Square (0,-1) Yellow,
  Square (0,0) Yellow,
  Square (0,1) Yellow,
  Square (-1,0) Yellow]
genBlock S = Block S (0,0) [
  Square (0,-1) Green,
  Square (0,0) Green,
  Square (1,0) Green,
  Square (1,1) Green]
genBlock Z = Block Z (0,0) [
  Square (0,-1) Blue,
  Square (0,0) Blue,
  Square (-1,0) Blue,
  Square (-1,1) Blue]
genBlock O = Block O (0,0) [
  Square (0,0) Indigo,
  Square (0,1) Indigo,
  Square (1,1) Indigo,
  Square (1,0) Indigo]
genBlock T = Block T (0,0) [
  Square (0,0) Violet,
  Square (-1,0) Violet,
  Square (0,1) Violet,
  Square (1,0) Violet]

main :: IO ()
main = do
  putStrLn "hi"
  gen <- getStdGen
  putStrLn $ show . (take 14) . blockList $ gen


