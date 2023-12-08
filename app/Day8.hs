module Day8 where

import Control.Arrow ((&&&))
import Control.Monad (foldM)
import Data.List (isSuffixOf)
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Void
import Debug.Trace
import System.IO
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

execute :: IO (Int, Int)
execute = do
  file <- openFile "app/inputs/day8.txt" ReadMode
  ll <- lines <$> hGetContents file
  let input = parseForPart1 ll
  return (part2 input, 0)

parseForPart1 :: [String] -> (String, M.Map String (String, String))
parseForPart1 = (cycle . head) &&& parseGraph
  where
    parseGraph = M.fromList . map (fromJust . parseMaybe pSingleGraph) . drop 2

    pSingleGraph :: Parser (String, (String, String))
    pSingleGraph = do
      key <- count 3 anySingle
      left <- string " = (" *> count 3 anySingle <* string ", "
      right <- count 3 anySingle <* string ")"
      return (key, (left, right))

part2 :: (String, M.Map String (String, String)) -> Int
part2 (directions, graph) =
  let endingsA = filter (isSuffixOf "A") $ M.keys graph
   in length $
        takeWhile (not . all (isSuffixOf "Z")) $
          scanl
            ( \nodes dir ->
                map
                  ( \node ->
                      let curNode = fromJust $ M.lookup node graph
                       in if dir == 'L' then fst curNode else snd curNode
                  )
                  nodes
            )
            endingsA
            directions
