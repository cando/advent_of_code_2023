{-# LANGUAGE BangPatterns #-}

module Day8 where

import Control.Arrow ((&&&))
import Control.DeepSeq (force)
import Data.Foldable
import Data.List (isSuffixOf)
import Data.Map.Strict qualified as M
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
  return (part2lcm input, 0)

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

part2BruteForce :: (String, M.Map String (String, String)) -> Int
part2BruteForce (directions, graph) =
  let endingsA = filter (isSuffixOf "A") $! M.keys graph
      (_, fCount) =
        foldl'
          ( \(nodes, !countTot) !dir ->
              let newNodes =
                    force $
                      map
                        ( \node ->
                            let curNode = graph M.! node
                             in if dir == 'L' then fst curNode else snd curNode
                        )
                        nodes
               in if all (isSuffixOf "Z") newNodes then error (show $ countTot + 1) else (newNodes, trace (show $ countTot + 1) (countTot + 1))
          )
          (endingsA, 0)
          directions
   in fCount

part2lcm :: (String, M.Map String (String, String)) -> Int
part2lcm (directions, graph) =
  let endingsA = filter (isSuffixOf "A") $ M.keys graph
      go =
        map
          ( \n ->
              length $
                takeWhile (not . isSuffixOf "Z") $
                  scanl
                    ( \node dir ->
                        let curNode = fromJust $ M.lookup node graph
                         in if dir == 'L' then fst curNode else snd curNode
                    )
                    n
                    directions
          )
          endingsA
   in foldl' lcm 1 go
