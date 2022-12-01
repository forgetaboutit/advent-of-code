module Day1 (run) where

import Data.List (sortOn)
import Data.Ord (Down (Down))
import Data.Text qualified as T
import Data.Text.IO qualified as TIO

run :: IO ()
run = do
  groups <- sumByGroup <$> readGroups

  putStr "Total calories of the most loaded elf: "
  print $ part1 groups

  putStr "Total calories of the three most loaded elves: "
  print $ part2 groups

part1 :: [Int] -> Int
part1 = maximum

part2 :: [Int] -> Int
part2 = sum . take 3 . sortOn Down

sumByGroup :: [[Int]] -> [Int]
sumByGroup = fmap sum

readGroups :: IO [[Int]]
readGroups = do
  groupLines <$> TIO.readFile "./inputs/day1.txt"
  where
    groupLines = parseNestedInts . split "" . T.splitOn "\n"
    parseNestedInts = (map . map) (read . T.unpack)

-- Shamelessly stolen from xmonad-contrib: XMonad.Prompt.Shell.split
split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split e l =
  f : split e (drop 1 ls)
  where
    (f, ls) = span (/= e) l