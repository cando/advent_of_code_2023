{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day5 where

import Control.Arrow ((&&&))
import Data.List
import Data.List.Split
import System.IO

execute :: IO (Int, Int)
execute = do
  file <- openFile "app/inputs/day5.txt" ReadMode
  (part1 &&& part2) <$> hGetContents file

part1 :: String -> Int
part1 input =
  let ll = lines input
      seeds = map read . drop 1 . words . head $ ll
      maps = map xToYMap . splitWhen (== "") . drop 2 $ ll
   in minimum $ map (`findLocation` maps) seeds

part2 :: String -> Int
part2 input = do
  let ll = lines input
      seeds = rangify . map read . drop 1 . words . head $ ll
      maps = map xToYMap . splitWhen (== "") . drop 2 $ ll
   in minimum $ map (`findLocation` maps) seeds

findLocation :: Int -> [[(Int, Int, Int)]] -> Int
findLocation = foldl doMap
  where
    doMap acc m = maybe acc (\(s, d, _) -> d + (acc - s)) $ find (\(s, _, c) -> s <= acc && s + c > acc) m

xToYMap :: [String] -> [(Int, Int, Int)]
xToYMap = map (tuple3 . (map read . words)) . drop 1

tuple3 :: [Int] -> (Int, Int, Int)
tuple3 [dest, source, num] = (source, dest, num)
tuple3 _ = error "incomplete pattern"

rangify :: [Int] -> [Int]
rangify = concatMap (\[s, e] -> [s .. s + e - 1]) . chunksOf 2
