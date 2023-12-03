module Main where

import Day1 qualified
import Day2 qualified

main :: IO ()
main =
  do
    doDay 1 Day1.execute
    doDay 2 Day2.execute
  where
    doDay :: Int -> IO Int -> IO ()
    doDay n day = do
      putStrLn "--------"
      putStrLn $ "Day " ++ show n
      putStrLn ""
      day >>= print
      putStrLn "--------"
