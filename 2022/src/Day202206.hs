module Day202206 (run) where

import Data.List (nub)
import Data.List.Index (indexed)
import Data.Maybe (mapMaybe)

run :: IO ()
run = do
  input <- readInput
  print $ fst $ head $ findStartOfPacketMarker 4 input
  print $ fst $ head $ findStartOfPacketMarker 14 input

findStartOfPacketMarker :: Int -> String -> [(Int, String)]
findStartOfPacketMarker targetLength xs = mapMaybe isMarker indexedWindows
  where
    isMarker (idx, part) =
      if length (nub part) == length part
        then Just (idx + targetLength, part)
        else Nothing

    indexedWindows = indexed $ windowed targetLength xs

-- According to https://www.markhneedham.com/blog/2012/02/28/haskell-creating-a-sliding-window-over-a-collection/
windowed :: Int -> [a] -> [[a]]
windowed size ls =
  case ls of
    [] -> []
    x : xs ->
      if length ls >= size
        then take size ls : windowed size xs
        else windowed size xs

readInput :: IO String
readInput = readFile "./inputs/day202206.txt"