module Day10 where

import Data.Maybe
import Data.Vector ((!))
import Data.Vector qualified as V
import Debug.Trace
import System.IO

data Direction = N | E | S | W deriving (Show)

execute :: IO (Int, Int)
execute = do
  file <- openFile "app/inputs/day10.txt" ReadMode
  grid <- V.fromList . concat . lines <$> hGetContents file
  return (part1 grid, part2 grid)

part1 :: V.Vector Char -> Int
part1 m =
  let start = fromJust $ V.findIndex (== 'S') m
   in go (start, N) m 0

go :: (Int, Direction) -> V.Vector Char -> Int -> Int
go (pos, dir) m count
  | curPos == 'S' && count > 0 = div count 2
  | curPos == 'S' = go (newPos '|' dir) m (count + 1)
  | otherwise = go (newPos curPos dir) m (count + 1)
  where
    curPos = m ! pos

    -- \| is a vertical pipe connecting north and south.
    -- - is a horizontal pipe connecting east and west.
    -- L is a 90-degree bend connecting north and east.
    -- J is a 90-degree bend connecting north and west.
    -- 7 is a 90-degree bend connecting south and west.
    -- F is a 90-degree bend connecting south and east.
    -- . is ground; there is no pipe in this tile.
    newPos '|' N = (pos - nCol, N)
    newPos '|' S = (pos + nCol, S)
    newPos '-' E = (pos + 1, E)
    newPos '-' W = (pos - 1, W)
    newPos 'L' S = (pos + 1, E)
    newPos 'L' W = (pos - nCol, N)
    newPos 'J' S = (pos - 1, W)
    newPos 'J' E = (pos - nCol, N)
    newPos '7' N = (pos - 1, W)
    newPos '7' E = (pos + nCol, S)
    newPos 'F' N = (pos + 1, E)
    newPos 'F' W = (pos + nCol, S)
    newPos '.' _ = error "whoops"
    newPos p d = error $ show pos ++ ": " ++ [p] ++ " " ++ show d

part2 :: V.Vector Char -> Int
part2 _ = 0

nCol :: Int
nCol = 140
