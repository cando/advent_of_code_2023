module Main where

import Day1 qualified

main :: IO ()
main =
  do
    doDay 1 Day1.execute
  where
    doDay :: Int -> IO Int -> IO ()
    doDay n day = do
      putStrLn "--------"
      putStrLn $ "Day " ++ show n
      putStrLn ""
      day >>= print
      putStrLn "--------"
