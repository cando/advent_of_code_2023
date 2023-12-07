module Main where

-- import Day1 qualified
-- import Day2 qualified
-- import Day3 qualified
-- import Day4 qualified
import Day7 qualified

main :: IO ()
main =
  do
    -- doDay 1 Day1.execute
    -- doDay 2 Day2.execute
    -- doDay 3 Day3.execute
    -- doDay 4 Day4.execute
    doDay 7 Day7.execute
  where
    doDay :: Int -> IO (Int, Int) -> IO ()
    doDay n day = do
      putStrLn "--------"
      putStrLn $ "Day " ++ show n
      putStrLn ""
      day >>= print
      putStrLn "--------"
