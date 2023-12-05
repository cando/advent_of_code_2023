module Day4 (execute) where

import Control.Monad (void)
import Data.List (intersect)
import Data.Map qualified as M
import Data.Void
import System.IO
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void String

-- 6420979
execute :: IO Int
execute = do
  file <- openFile "app/inputs/day4.txt" ReadMode
  cards <- M.fromList . zip [1 ..] . map handleGame . lines <$> hGetContents file
  let newCards = conc cards (M.elems cards)
  return $ length newCards
  where
    conc dict =
      concatMap
        (\cs -> cs : conc dict (insertCards (cid cs) (numCopies cs) [] dict))

    insertCards :: Int -> Int -> [Card] -> M.Map Int Card -> [Card]
    insertCards sid num acc cards
      | num == 0 = acc
      | otherwise = insertCards sid (num - 1) (maybe acc (: acc) (M.lookup (sid + num) cards)) cards

    handleGame s = case runParser pLine "" s of
      Left a -> error (errorBundlePretty a)
      Right n -> n

-- let result = concatMap (\c@MkCard {numCopies = numCopies} -> replicate numCopies c) cards
-- return $ trace (show result) (length result)
-- Part 1
-- pGame :: Parser Int
-- pGame = do
--   sum <$> many pLine

-- pLine :: Parser Int
-- pLine = do
--   _ <- manyTill anySingle (char ':')
--   winning <- decParser
--   own <- decParser
--   let combined = winning `intersect` own
--   if not (null combined)
--     then return $ (\l -> 2 ^ (length l - 1)) combined
--     else return 0
--   where
--     decParser = manyTill (some spaceChar *> decimal) (void (string " |") <|> eof) :: Parser [Int]

data Card = MkCard
  { cid :: Int,
    win :: [Int],
    own :: [Int],
    numCopies :: Int
  }

instance Show Card where
  show (MkCard {cid = cid}) = show cid

pLine :: Parser Card
pLine = do
  cid <- string "Card" *> space *> some numberChar <* char ':'
  winning <- decParser
  own <- decParser
  return MkCard {cid = read cid, win = winning, own = own, numCopies = length $ intersect winning own}
  where
    decParser = manyTill (some spaceChar *> decimal) (void (string " |") <|> eof) :: Parser [Int]
