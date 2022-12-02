module Main (main) where

import Day1 qualified
import Day2 qualified

main :: IO ()
main = do
  runDay 1 Day1.run
  runDay 2 Day2.run

runDay :: Int -> IO () -> IO ()
runDay day run = do
  putStrLn dayText
  putStrLn dayUnderline
  run
  putStrLn ""

  where
    dayText = "Day " ++ show day
    dayUnderline = replicate (length dayText) '='