module Day5 (execute) where

import Data.List
import Data.List.Split
import System.IO

execute :: IO Int
execute = do
  file <- openFile "app/inputs/day5.txt" ReadMode
  ll <- lines <$> hGetContents file
  let seeds = map read . drop 1 . words . head $ ll
  let maps = map xToYMap . splitWhen (== "") . drop 2 $ ll
  return $ minimum $ map (`findLocation` maps) seeds

findLocation :: Int -> [[(Int, Int, Int)]] -> Int
findLocation = foldl doMap
  where
    doMap acc m = maybe acc (\(s, d, _) -> d + (acc - s)) $ find (\(s, _, c) -> s <= acc && s + c >= acc) m

xToYMap :: [String] -> [(Int, Int, Int)]
xToYMap = map (tuple3 . (map read . words)) . drop 1

tuple3 :: [Int] -> (Int, Int, Int)
tuple3 [dest, source, num] = (source, dest, num)
tuple3 _ = error "incomplete pattern"

-- range :: [Int] -> [(Int, Int)]
-- range [dest, source, num] = zip [source .. source + num] [dest .. dest + num]
-- range _ = error "incomplete pattern"
