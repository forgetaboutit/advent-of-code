module Day202203 (run) where

import Data.Char (isAsciiLower, isAsciiUpper)
import Data.List (nub)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHC.Base (ord)
import GHC.OldList (intersect)

run :: IO ()
run = do
  input <- readInput

  print $ sumGroupedPriorities $ commonPrioritiesInPair . parseHalfLines <$> input
  print $ sumGroupedPriorities $ commonPrioritiesInGroup <$> chunk 3 input

sumGroupedPriorities :: Foldable t => t [Char] -> Int
sumGroupedPriorities allGroups = sum $ concatMap (fmap priority) allGroups

type LineHalves = (T.Text, T.Text)

commonPrioritiesInPair :: LineHalves -> [Char]
commonPrioritiesInPair = nub . common
  where
    common (l, r) = T.unpack l `intersect` T.unpack r

commonPrioritiesInGroup :: [T.Text] -> [Char]
commonPrioritiesInGroup [] = []
commonPrioritiesInGroup [x] = T.unpack x
commonPrioritiesInGroup (x : xs) =
  nub $ T.unpack x `intersect` commonPrioritiesInGroup xs

priority :: Char -> Int
priority c | isAsciiLower c = ord c - 96
priority c | isAsciiUpper c = ord c - 38
priority unknown = error $ "Undefined priority for char " ++ show unknown

parseHalfLines :: T.Text -> LineHalves
parseHalfLines line = T.splitAt (T.length line `div` 2) line

readInput :: IO [T.Text]
readInput = T.lines <$> TIO.readFile "./inputs/day202203.txt"

-- Stolen from Documentation.SBV.Examples.Puzzles.MagicSquare.chunk
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk i xs = let (f, r) = splitAt i xs in f : chunk i r
