module Day12 where

import Control.Arrow ((&&&))
import Data.Bifunctor (second)
import Data.List (group)
import Data.List.Extra (splitOn)
import System.IO

execute :: IO (Int, Int)
execute = do
  file <- openFile "app/inputs/day12.txt" ReadMode
  measures <- map (second (map read . splitOn ",") . (head &&& last) . words) . lines <$> hGetContents file
  return (part1 measures, part2 measures)

part1 :: [(String, [Int])] -> Int
part1 = sum . map (\(s, i) -> countSolutions i s)

countSolutions :: [Int] -> [Char] -> Int
countSolutions groups input = length $ filter (isValid groups) $ generateSolutions input "" []

isValid :: [Int] -> String -> Bool
isValid [] _ = True
isValid i s = i == (map length . filter (all (== '#')) . group) s

generateSolutions :: [Char] -> [Char] -> [[Char]] -> [[Char]]
generateSolutions [] s acc = acc ++ [s]
generateSolutions ('?' : xs) s acc = generateSolutions xs (s ++ ['#']) acc ++ generateSolutions xs (s ++ ['.']) acc
generateSolutions (c : xs) s acc = generateSolutions xs (s ++ [c]) acc

valid :: [Int] -> [Char] -> Int -> Int
valid (1 : ixs) ('?' : '.' : sxs) acc = valid ixs sxs (acc + 1)
valid (1 : ixs) ('.' : '?' : sxs) acc = valid ixs sxs (acc + 1)
valid (1 : ixs) ('?' : '?' : sxs) acc = valid ixs sxs (acc + 1)
valid (1 : ixs) ('#' : _ : sxs) acc = valid ixs sxs acc
valid (1 : ixs) (_ : '#' : sxs) acc = valid ixs sxs acc
valid (2 : ixs) ('?' : '?' : '.' : sxs) acc = valid ixs sxs (acc + 1)
valid (2 : ixs) ('.' : '?' : '?' : sxs) acc = valid ixs sxs (acc + 1)
valid (2 : ixs) ('#' : '?' : '?' : sxs) acc = valid ixs sxs (acc + 1)
valid (2 : ixs) ('#' : '?' : '.' : sxs) acc = valid ixs sxs (acc + 1)
valid (2 : ixs) ('?' : '#' : '.' : sxs) acc = valid ixs sxs (acc + 1)
valid (2 : ixs) ('#' : '#' : '.' : sxs) acc = valid ixs sxs acc
valid (2 : ixs) ('#' : '#' : '?' : sxs) acc = valid ixs sxs acc
valid (2 : ixs) ('.' : '#' : '#' : '.' : sxs) acc = valid ixs sxs acc
valid (2 : ixs) ('?' : '#' : '#' : '?' : sxs) acc = valid ixs sxs acc
valid [] _ acc = acc
valid _ _ _ = undefined

part2 :: [(String, [Int])] -> Int
part2 _ = 0
