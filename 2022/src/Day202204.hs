{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day202204 (run) where

import Data.Bifunctor (Bifunctor, bimap)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO

run :: IO ()
run = do
  input <- readInput
  let ranges = parseRange <$> input
  let containedRanges = countMatchingAssignments containsAny ranges
  print containedRanges

  let overlappingRanges = countMatchingAssignments overlapsAny ranges
  print overlappingRanges

countMatchingAssignments :: (Range -> Range -> Bool) -> [(Range, Range)] -> Int
countMatchingAssignments predicate = length . filter (uncurry predicate)

type Range = (Int, Int)

containsAny :: Range -> Range -> Bool
containsAny rng1 rng2 = contains rng1 rng2 || contains rng2 rng1
  where
    contains (l1, r1) (l2, r2) = l1 <= l2 && r2 <= r1

overlapsAny :: Range -> Range -> Bool
overlapsAny rng1 rng2 = overlaps rng1 rng2 || overlaps rng2 rng1
  where
    overlaps (_, r1) (l2, r2) = l2 <= r1 && r1 <= r2

parseRange :: T.Text -> (Range, Range)
parseRange = parseInts . parseRangeTexts . parsePair
  where
    parsePair = splitIntoPair ","
    parseRangeTexts = both $ splitIntoPair "-"
    parseInts = (both . both) $ read . T.unpack
    splitIntoPair sep text =
      let [f, s] = T.splitOn sep text
       in (f, s)

readInput :: IO [T.Text]
readInput = T.lines <$> TIO.readFile "./inputs/day202204.txt"

both :: (Bifunctor f) => (a -> b) -> f a a -> f b b
both f = bimap f f