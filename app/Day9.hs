module Day9 where

import Data.List.Extra
import Debug.Trace
import System.IO

execute :: IO (Int, Int)
execute = do
  file <- openFile "app/inputs/day9.txt" ReadMode
  measures <- map (map read . words) . lines <$> hGetContents file
  return (part1 measures, part2 measures)

part1 :: [[Int]] -> Int
part1 = sum . map (\l -> go1 [l] l)

solveSingle :: [(Int, Int)] -> [Int]
solveSingle = foldl' (\acc (a, b) -> acc ++ [b - a]) []

toPairs :: [Int] -> [(Int, Int)]
toPairs inp = zip inp (drop 1 inp)

go1 :: [[Int]] -> [Int] -> Int
go1 acc inp
  | all (== 0) inp = sum (map last acc)
  | otherwise =
      let solved = solveSingle (toPairs inp)
       in go1 (acc ++ [solved]) solved

part2 :: [[Int]] -> Int
part2 = sum . map (\l -> go2 [l] l)

go2 :: [[Int]] -> [Int] -> Int
go2 acc inp
  | all (== 0) inp = sum (zipWith (\i v -> (if odd i then -1 * v else v)) [0 ..] (map head acc))
  | otherwise =
      let solved = solveSingle (toPairs inp)
       in go2 (acc ++ [solved]) solved
