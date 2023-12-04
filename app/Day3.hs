module Day3 (execute) where

import Data.Char
import Data.Map.Strict qualified as M
import Data.Matrix
import Data.Maybe (isJust)
import Debug.Trace qualified as Debug
import System.IO

execute :: IO Int
execute = do
  file <- openFile "app/inputs/day3.txt" ReadMode
  handleGame . lines <$> hGetContents file

-- Part 1
-- handleGame :: [[Char]] -> Int
-- handleGame m =
--   let mat = fromLists m
--       run =
--         foldl
--           (doRunD mat)
--           (0, "", 1, 1, False) -- sum, cur num, cur r, cur c, curNum should be considered
--           mat
--       fst' (s, _, _, _, _) = s
--    in fst' run
--   where
--     doRunD mat (totalSum, curNum, r, c, valid) el =
--       let res = doRun mat (totalSum, curNum, r, c, valid) el
--        in Debug.trace (show res) res

--     doRun mat (totalSum, curNum, r, c, valid) el
--       | isDigit el = next (totalSum, curNum ++ [el], r, c, valid || neighToSymbol mat r c)
--       | curNum /= "" && valid = next (totalSum + read curNum :: Int, "", r, c, False)
--       | otherwise = next (totalSum, "", r, c, False)

--     next (a, b, r, c, v)
--       | c == 140 && b /= "" && v = (a + read b :: Int, "", r + 1, 1, False)
--       | c == 140 = (a, b, r + 1, 1, v)
--       | otherwise = (a, b, r, c + 1, v)

--     isEngineSym c = c /= '.' && not (isDigit c)

--     neighToSymbol :: Matrix Char -> Int -> Int -> Bool
--     neighToSymbol ma r c
--       | (isEngineSym <$> safeGet r (c - 1) ma) == Just True = True
--       | (isEngineSym <$> safeGet (r + 1) (c - 1) ma) == Just True = True
--       | (isEngineSym <$> safeGet (r + 1) c ma) == Just True = True
--       | (isEngineSym <$> safeGet (r + 1) (c + 1) ma) == Just True = True
--       | (isEngineSym <$> safeGet r (c + 1) ma) == Just True = True
--       | (isEngineSym <$> safeGet (r - 1) (c + 1) ma) == Just True = True
--       | (isEngineSym <$> safeGet (r - 1) c ma) == Just True = True
--       | (isEngineSym <$> safeGet (r - 1) (c - 1) ma) == Just True = True
--       | otherwise = False

data Turn
  = Turn1
  | Turn2
  deriving (Eq, Show)

-- Part 2
handleGame :: [[Char]] -> Int
handleGame m =
  let mat = fromLists m
      run =
        foldl
          (doRunD mat)
          (M.empty, "", 1, 1, Nothing) -- sum, cur num, cur r, cur c, curNum should be considered
          mat
      fst' (s, _, _, _, _) = s
   in M.foldl (\acc gs -> acc + product gs) 0 $ M.filter (\gs -> length gs == 2) $ fst' run
  where
    doRunD mat (gearMap, curNum, r, c, lastGear) el =
      let res = doRun mat (gearMap, curNum, r, c, lastGear) el
       in Debug.trace (show res) res

    doRun mat (gearMap, curNum, r, c, lastGear) el
      | isDigit el = next (gearMap, curNum ++ [el], r, c, if isJust lastGear then lastGear else neighToGear mat r c)
      | curNum /= "" && isJust lastGear = next (M.insertWith (++) lastGear [read curNum :: Int] gearMap, "", r, c, Nothing)
      | otherwise = next (gearMap, "", r, c, Nothing)

    next (a, b, r, c, g)
      | c == 140 && b /= "" && isJust g = (M.insertWith (++) g [read b :: Int] a, "", r + 1, 1, Nothing)
      | c == 140 = (a, b, r + 1, 1, g)
      | otherwise = (a, b, r, c + 1, g)

    isGearSym = (==) '*'

    neighToGear :: Matrix Char -> Int -> Int -> Maybe (Int, Int)
    neighToGear ma r c
      | (isGearSym <$> safeGet r (c - 1) ma) == Just True = Just (r, c - 1)
      | (isGearSym <$> safeGet (r + 1) (c - 1) ma) == Just True = Just (r + 1, c - 1)
      | (isGearSym <$> safeGet (r + 1) c ma) == Just True = Just (r + 1, c)
      | (isGearSym <$> safeGet (r + 1) (c + 1) ma) == Just True = Just (r + 1, c + 1)
      | (isGearSym <$> safeGet r (c + 1) ma) == Just True = Just (r, c + 1)
      | (isGearSym <$> safeGet (r - 1) (c + 1) ma) == Just True = Just (r - 1, c + 1)
      | (isGearSym <$> safeGet (r - 1) c ma) == Just True = Just (r - 1, c)
      | (isGearSym <$> safeGet (r - 1) (c - 1) ma) == Just True = Just (r - 1, c - 1)
      | otherwise = Nothing
