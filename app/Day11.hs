module Day11 where

-- import Data.Matrix

import Data.Foldable (Foldable (foldl'), foldMap')
import Data.List.Extra (transpose)
import Debug.Trace
import System.IO

execute :: IO (Int, Int)
execute = do
  file <- openFile "app/inputs/day11.txt" ReadMode
  grid <- transpose . expandRows . transpose . expandRows . lines <$> hGetContents file
  return (part1 grid, part2 grid)
  where
    expandRows = concatMap (\l -> if all (== '.') l then [l, l] else [l])

part1 :: [String] -> Int
part1 g = sum $ foldl' (\acc (gi, el) -> acc ++ mmh el gi) [] findGal
  where
    len = length g
    findGal = zip [0 ..] $ foldMap' (\(r, row) -> foldMap' (\(c, el) -> ([(r, c) | el == '#'])) $ zip [0 .. length row - 1] row) $ zip [0 .. len - 1] g
    neighGals startIndex = filter (\(i, _) -> i > startIndex) findGal
    mmh start startIndex = manhattan start (neighGals startIndex) []

    manhattan :: (Int, Int) -> [(Int, (Int, Int))] -> [Int] -> [Int]
    manhattan _ [] acc = acc
    manhattan s@(x1, y1) ((_, (x2, y2)) : xs) acc = acc ++ [abs (x1 - x2) + abs (y1 - y2)] ++ manhattan s xs acc

part2 :: [[Char]] -> Int
part2 _ = 0
