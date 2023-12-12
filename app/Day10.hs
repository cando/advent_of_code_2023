module Day10 where

import Data.Foldable
import Data.Maybe
import Data.Vector ((!))
import Data.Vector qualified as V
import Debug.Trace
import System.IO

data Direction = N | E | S | W deriving (Show, Eq)

execute :: IO (Int, Int)
execute = do
  file <- openFile "app/inputs/day10.txt" ReadMode
  grid <- V.fromList . concat . lines <$> hGetContents file
  return (part1 grid, part2 grid)

part1 :: V.Vector Char -> Int
part1 m =
  let start = fromJust $ V.findIndex (== 'S') m
      (c, _) = go (start, N) m 0 start [] []
   in c

countIn :: [[Int]] -> [(Int, Int)] -> V.Vector Char -> Int
countIn segV segH m = foldl' (\c i -> if isInside i then c + 1 else c) 0 [0 .. length m - 1]
  where
    isOnPath p = any (\(s, e) -> s <= p && p <= e) segH || any (elem p) segV
    isInside p =
      let (_, r) = quotRem p nCol
          numIntersect = length $ filter (\l -> r > rem (head l) nCol && head l < p && p < last l) segV
       in not (isOnPath p) && trace (show p ++ ": " ++ show numIntersect) (odd numIntersect)

go :: (Int, Direction) -> V.Vector Char -> Int -> Int -> [[Int]] -> [(Int, Int)] -> (Int, ([[Int]], [(Int, Int)]))
go (pos, dir) m count startSegPos segV segH
  | curPos == 'S' && count > 0 =
      let nSegV = if dir == N || dir == S then segV ++ [genV (min startSegPos pos) (max startSegPos pos)] else segV
          nSegH = if dir == E || dir == W then segH ++ [(min startSegPos pos, max startSegPos pos)] else segH
       in (div count 2, (nSegV, nSegH))
  | curPos == 'S' = go (newPos '|' dir) m (count + 1) startSegPos segV segH
  | otherwise =
      let (np, nd) = newPos curPos dir
          nPos = if (dir == N || dir == S) && (nd == E || nd == W) || (dir == E || dir == W) && (nd == N || nd == S) then pos else startSegPos
          nSegV = if (dir == N || dir == S) && (nd == E || nd == W) then segV ++ [genV (min startSegPos pos) (max startSegPos pos)] else segV
          nSegH = if (dir == E || dir == W) && (nd == N || nd == S) then segH ++ [(min startSegPos pos, max startSegPos pos)] else segH
       in go (np, nd) m (count + 1) nPos nSegV nSegH
  where
    curPos = m ! pos
    genV s e = [s, s + nCol .. e]
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
part2 m =
  let start = fromJust $ V.findIndex (== 'S') m
      (_, (sv, sh)) = go (start, N) m 0 start [] []
   in trace (show (sv, sh)) countIn sv sh m

nCol :: Int
nCol = 140
