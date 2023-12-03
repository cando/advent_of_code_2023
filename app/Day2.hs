module Day2 (execute) where

import Control.Monad (join, void)
import Data.Void
import Debug.Trace
import System.IO
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

data Color = Red Int | Green Int | Blue Int

execute :: IO Int
execute = do
  file <- openFile "app/inputs/day2.txt" ReadMode
  sum . map handleGame . lines <$> hGetContents file
  where
    handleGame s = case runParser pGame "" s of
      Left a -> error (errorBundlePretty a)
      Right n -> n

-- Part 1
-- pGame :: Parser Int
-- pGame = do
--   gids <- string "Game " *> some numberChar <* char ':'
--   let gid = read gids :: Int
--   rounds <- join <$> some pRound
--   let (b, r, g) =
--         foldl
--           ( \(a1, a2, a3) el -> case el of
--               Blue n -> (n <= 14 && a1, a2, a3)
--               Red n -> (a1, a2 && n <= 12, a3)
--               Green n -> (a1, a2, a3 && n <= 13)
--           )
--           (True, True, True)
--           rounds
--   let num = if b && r && g then gid else 0
--   return $ trace (show gid ++ ": " ++ show num) num

-- Part 2
pGame :: Parser Int
pGame = do
  gids <- string "Game " *> some numberChar <* char ':'
  let gid = read gids :: Int
  rounds <- join <$> some pRound
  let (b, r, g) =
        foldl
          ( \(a1, a2, a3) el -> case el of
              Blue n -> (max a1 n, a2, a3)
              Red n -> (a1, max a2 n, a3)
              Green n -> (a1, a2, max a3 n)
          )
          (0, 0, 0)
          rounds
  return $ trace (show gid ++ ": " ++ show (b * r * g)) b * r * g

pRound :: Parser [Color]
pRound =
  do
    some
      ( try (pColor Blue "blue")
          <|> try (pColor Red "red")
          <|> try (pColor Green "green")
          <|> ( Red 0
                  <$ char ','
              )
      )
    <* (void (char ';') <|> eof)

pColor :: (Int -> Color) -> String -> Parser Color
pColor c s = do
  n <- spaceChar *> some numberChar <* space <* string s
  return $ c (read n :: Int)
