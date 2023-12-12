module Day11 where

-- import Data.Matrix

import Data.Foldable (Foldable (foldl'), foldMap')
import Data.List.Extra (transpose)
import System.IO

execute :: IO (Int, Int)
execute = do
  file <- openFile "app/inputs/day11.txt" ReadMode
  content <- hGetContents file
  let grid1 = transpose . expandRows . transpose . expandRows . lines $ content
  let grid2 = lines content
  return (part1 grid1, part2 grid2)
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

part2 :: [String] -> Int
part2 g = sum $ foldl' (\acc (gi, el) -> acc ++ mmh el gi) [] findGal
  where
    len = length g
    findGal = zip [0 ..] $ foldMap' (\(r, row) -> foldMap' (\(c, el) -> ([(c, r) | el == '#'])) $ zip [0 .. length row - 1] row) $ zip [0 .. len - 1] g
    neighGals startIndex = filter (\(i, _) -> i > startIndex) findGal
    mmh start startIndex = manhattan start (neighGals startIndex) []

    manhattan :: (Int, Int) -> [(Int, (Int, Int))] -> [Int] -> [Int]
    manhattan _ [] acc = acc
    manhattan s@(x1, y1) ((_, (x2, y2)) : xs) acc =
      acc ++ [abs (x1 - x2) + abs (y1 - y2) + numEmptyRow y1 y2 + numEmptyColumns x1 x2] ++ manhattan s xs acc

    numEmptyRow y1 y2 = 
      let nn = length $ filter (all (== '.')) l
      in nn * 1000000 - 1 * nn
      where
        l = take (abs (y2 - y1)) $ drop (y1 + 1) g

    numEmptyColumns x1 x2 = 
      let nn = length $ filter (all (== '.')) l
      in nn * 1000000 - 1 * nn
      where
        l = take (abs (x2 - x1)) $ drop (min x1 x2) (transpose g)
