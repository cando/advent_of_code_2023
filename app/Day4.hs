module Day4 (execute) where

import Control.Monad (void)
import Data.List (intersect)
import Data.Void
import Debug.Trace qualified as Debug
import System.IO
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void String

execute :: IO Int
execute = do
  file <- openFile "app/inputs/day4.txt" ReadMode
  sum . map handleGame . lines <$> hGetContents file
  where
    handleGame s = case runParser pGame "" s of
      Left a -> error (errorBundlePretty a)
      Right n -> n

-- Part 1
pGame :: Parser Int
pGame = do
  sum <$> many pLine

pLine :: Parser Int
pLine = do
  _ <- manyTill anySingle (char ':')
  winning <- decParser
  own <- decParser
  let combined = winning `intersect` own
  if not (null combined)
    then return $ (\l -> 2 ^ (length l - 1)) combined
    else return 0
  where
    decParser = manyTill (some spaceChar *> decimal) (void (string " |") <|> eof) :: Parser [Int]
