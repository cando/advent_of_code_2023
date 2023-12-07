module Day7 where

import Data.Bifunctor (Bifunctor (first), bimap)
import Data.Function (on)
import Data.List (sort, sortBy)
import Data.List.Extra (group, nub)
import System.IO

data Card = A | K | Q | J | T | N9 | N8 | N7 | N6 | N5 | N4 | N3 | N2 | N1 deriving (Ord, Eq, Show)

data Hand = FiveOfAKind [Card] | FourOfAKind [Card] | FullHouse [Card] | ThreeOfAKind [Card] | TwoPair [Card] | OnePair [Card] | HighCard [Card]
  deriving (Ord, Eq, Show)

execute :: IO (Int, Int)
execute = do
  file <- openFile "app/inputs/day7.txt" ReadMode
  ll <- lines <$> hGetContents file
  let cards = map parseForPart1 ll
  return (part1 cards, 0)

parseForPart1 :: String -> ([Card], Int)
parseForPart1 = bimap (map toCard) read . toTuple . words

part1 :: [([Card], Int)] -> Int
part1 = sum . zipWith (*) [1 ..] . map snd . sortBy (flip compare `on` fst) . map (first detectHand)

detectHand :: [Card] -> Hand
detectHand cards
  | all (== head cards) cards = FiveOfAKind cards
  | (length . filter (\g -> length g == 4) . group . sort) cards == 1 = FourOfAKind cards
  | (length . group . sort) cards == 2 = FullHouse cards
  | (length . filter (\g -> length g == 3) . group . sort) cards == 1 = ThreeOfAKind cards
  | (length . filter (\g -> length g == 2) . group . sort) cards == 2 = TwoPair cards
  | (length . nub) cards == 4 = OnePair cards
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
toCard '9' = N9
toCard '8' = N8
toCard '7' = N7
toCard '6' = N6
toCard '5' = N5
toCard '4' = N4
toCard '3' = N3
toCard '2' = N2
toCard '1' = N1
toCard _ = error "no card"
