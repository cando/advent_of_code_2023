module Day7 where

import Data.Bifunctor (Bifunctor (first), bimap)
import Data.Char (digitToInt)
import Data.Function (on)
import Data.List (sort, sortBy)
import Data.List.Extra (group, nub)
import Debug.Trace
import System.IO

data Card = A | K | Q | J | T | N Int deriving (Ord, Eq, Show)

data Hand = FiveOfAKind Card | FourOfAKind [Card] | ThreeOfAKing [Card] | FullHouse [Card] | TwoPair [Card] | OnePair [Card] | HighCard [Card]
  deriving (Ord, Eq, Show)

execute :: IO (Int, Int)
execute = do
  file <- openFile "app/inputs/day7.txt" ReadMode
  ll <- lines <$> hGetContents file
  let cards = map parseForPart1 ll
  return (part1 cards, 0)
  where
    parseForPart1 :: String -> ([Card], Int)
    parseForPart1 = bimap (map toCard) read . toTuple . words

part1 :: [([Card], Int)] -> Int
part1 i = sum $ trace (show $ r i) r i
  where
    r = zipWith (*) [1 ..] . map snd . sortBy (flip compare `on` fst) . map (first detectHand)

detectHand :: [Card] -> Hand
detectHand cards
  | all (== head cards) cards = FiveOfAKind $ head cards
  | (length . nub) cards == 2 = FourOfAKind cards
  | (length . nub) cards == 3 = ThreeOfAKing cards
  | (length . group . sort) cards == 2 = FullHouse cards
  | (length . group . sort) cards == 3 = TwoPair cards
  | (length . group . sort) cards == 4 = OnePair cards
  | (length . nub) cards == 5 = HighCard cards
  | otherwise = error $ show cards

part2 :: Int
part2 = 0

toTuple [hand, bid] = (hand, bid)
toTuple _ = error "input error"

toCard 'A' = A
toCard 'K' = K
toCard 'Q' = Q
toCard 'J' = J
toCard 'T' = T
toCard n = N $ digitToInt n
