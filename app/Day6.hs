module Day6 where

import Control.Arrow ((&&&))
import System.IO

execute :: IO (Int, Int)
execute = do
  file <- openFile "app/inputs/day6.txt" ReadMode
  ll <- lines <$> hGetContents file
  let races = zip (parse . head $ ll) (parse . last $ ll)
  return $ (part1 &&& part2) races
  where
    parse = map read . drop 1 . words

part1 :: [(Int, Int)] -> Int
part1 = foldl (\acc (time, distance) -> acc * findNumWinners time distance) 1
  where
    findNumWinners time distance = (length . filter (> distance)) [s * (time - s) | s <- [1 .. time]]

part2 :: [(Int, Int)] -> Int
part2 races =
  0
