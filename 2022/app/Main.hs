module Main (main) where

import Day1 qualified
import Day2 qualified
import Day202001 qualified
import Day202203 qualified
import Day202204 qualified
import Day202205 qualified
import Day202206 qualified

main :: IO ()
main = do
  run2020
  run2022

run2020 :: IO ()
run2020 = do
  printYearHeading 2020

  runDay 1 Day202001.run

run2022 :: IO ()
run2022 = do
  printYearHeading 2022
  runDay 1 Day1.run
  runDay 2 Day2.run
  runDay 3 Day202203.run
  runDay 4 Day202204.run
  runDay 5 Day202205.run
  runDay 6 Day202206.run

printYearHeading :: Int -> IO ()
printYearHeading year = do
  print year
  putStrLn $ textUnderline $ show year
  putStrLn ""

runDay :: Int -> IO () -> IO ()
runDay day run = do
  putStrLn dayText
  putStrLn $ textUnderline dayText
  run
  putStrLn ""
  where
    dayText = "Day " ++ show day

textUnderline :: String -> String
textUnderline text = replicate (length text) '='