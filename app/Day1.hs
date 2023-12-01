module Day1 (execute) where

import Control.Monad (join)
import Data.Char (digitToInt)
import Data.Maybe (fromMaybe)
import Data.Void
import System.IO
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

execute :: IO Int
execute = do
  file <- openFile "app/inputs/day1.txt" ReadMode
  sum . map (fromMaybe 0 . parseMaybe pLineNum) . lines <$> hGetContents file

pLineNum :: Parser Int
pLineNum = do
  n <- many $ try pWordNum <|> try ((\c -> [digitToInt c]) <$> digitChar) <|> ([0] <$ anySingle)
  let filtered = filter (/= 0) (join n)
  return $ head filtered * 10 + last filtered

pWordNum :: Parser [Int]
pWordNum =
  choice
    [ [8, 2] <$ string "eightwo",
      [8, 3] <$ string "eighthree",
      [7, 9] <$ string "sevenine",
      [2, 1] <$ string "twone",
      [1, 8] <$ string "oneight",
      [3, 8] <$ string "threeight",
      [5, 8] <$ string "fiveight",
      [9, 8] <$ string "nineight",
      [1] <$ string "one",
      [2] <$ string "two",
      [3] <$ string "three",
      [4] <$ string "four",
      [5] <$ string "five",
      [6] <$ string "six",
      [7] <$ string "seven",
      [8] <$ string "eight",
      [9] <$ string "nine"
    ]
