module Day202001 (run) where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO

run :: IO ()
run = do
  input <- readInput
  let expenses = parseExpenses input

  print $ findTwoEntries expenses
  print $ findThreeEntries expenses

findTwoEntries :: [Int] -> Int
findTwoEntries expenses =
  head [x * y | x <- expenses, y <- expenses, x + y == 2020]

findThreeEntries :: [Int] -> Int
findThreeEntries expenses =
  head [x * y * z | x <- expenses, y <- expenses, z <- expenses, x + y + z == 2020]

parseExpenses :: T.Text -> [Int]
parseExpenses input = read . T.unpack <$> T.splitOn "\n" input

readInput :: IO T.Text
readInput = TIO.readFile "./inputs/day202001.txt"
