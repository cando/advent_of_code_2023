module Day6 where

import Control.Arrow ((&&&))
import System.IO

execute :: IO (Int, Int)
execute = do
  file <- openFile "app/inputs/day6.txt" ReadMode
  ll <- lines <$> hGetContents file
  let races = zip (parseForPart1 . head $ ll) (parseForPart1 . last $ ll)
  let (time, distance) = ((parseForPart2 . head) &&& (parseForPart2 . last)) ll
  return (part1 races, part2 time distance)
  where
    parseForPart1 = map read . drop 1 . words
    parseForPart2 = read . concat . drop 1 . words

part1 :: [(Int, Int)] -> Int
part1 = product . map (uncurry findNumWinners)

part2 :: Int -> Int -> Int
part2 = findNumWinners

findNumWinners :: Int -> Int -> Int
findNumWinners time distance = (length . filter (> distance)) [s * (time - s) | s <- [1 .. time]]
