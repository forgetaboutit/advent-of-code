module Main (main) where

import Day1 qualified

main :: IO ()
main = do
  runDay 1 Day1.run

runDay :: Int -> IO () -> IO ()
runDay day run = do
  putStrLn dayText
  putStrLn dayUnderline
  run
  where
    dayText = "Day " ++ show day
    dayUnderline = replicate (length dayText) '='