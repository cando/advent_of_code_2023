module Day7 where

import Data.Bifunctor (Bifunctor (first), bimap)
import Data.Function (on)
import Data.List (sort, sortBy)
import Data.List.Extra (group)
import System.IO

data Card = A | K | Q | T | N9 | N8 | N7 | N6 | N5 | N4 | N3 | N2 | J deriving (Ord, Eq, Show)

data Hand = FiveOfAKind [Card] | FourOfAKind [Card] | FullHouse [Card] | ThreeOfAKind [Card] | TwoPair [Card] | OnePair [Card] | HighCard [Card]
  deriving (Ord, Eq, Show)

execute :: IO (Int, Int)
execute = do
  file <- openFile "app/inputs/day7.txt" ReadMode
  ll <- lines <$> hGetContents file
  let cards = map parseForPart2 ll
  return (part2 cards, 0)

parseForPart2 :: String -> ([Card], Int)
parseForPart2 = bimap (map toCard) read . toTuple . words

part2 :: [([Card], Int)] -> Int
part2 = sum . zipWith (*) [1 ..] . map snd . sortBy (flip compare `on` fst) . map (first handType)

handType :: [Card] -> Hand
handType cards =
  case sort $ map length $ group $ sort $ filter (/= J) cards of
    [] -> FiveOfAKind cards
    [_] -> FiveOfAKind cards
    [1, _] -> FourOfAKind cards
    [2, _] -> FullHouse cards
    [1, 1, _] -> ThreeOfAKind cards
    [1, 2, _] -> TwoPair cards
    [1, 1, 1, _] -> OnePair cards
    [1, 1, 1, 1, 1] -> HighCard cards
    _ -> undefined

toTuple :: [b] -> (b, b)
toTuple [hand, bid] = (hand, bid)
toTuple _ = error "input error"

toCard :: Char -> Card
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
toCard _ = error "no card"
