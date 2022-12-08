module Day202208 (run) where

import Data.List (singleton)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHC.Arr qualified as Array

run :: IO ()
run = do
  input <- readInput
  let forest = buildArrays input
  print $ countVisibleFromOutside forest

-- Outer array are the rows, inner arrays the columns
type Forest = Array.Array Int (Array.Array Int Int)

type InnerBounds = (Int, Int)

countVisibleFromOutside :: (InnerBounds, Forest) -> (Int, Int)
countVisibleFromOutside ((colFrom, colTo), forest) =
  (countVisibleTrees, highestScenicScore)
  where
    countVisibleTrees = length (filter isVisible innerSquare) + countOuterRim

    highestScenicScore = maximum (scenicScore <$> allTrees)

    scenicScore (x, y) =
      let height = treeAtPos x y
       in product $
            length . takeWhileHasView height
              <$> [ reverse $ treesNorth x y,
                    reverse $ treesWest x y,
                    treesEast x y,
                    treesSouth x y
                  ]

    takeWhileHasView _ [] = []
    takeWhileHasView height (x : xs)
      | x == height = singleton x
      | x > height = singleton x
      | x < height = x : takeWhileHasView height xs
      | otherwise = []

    isVisible (x, y) =
      let height = treeAtPos x y
       in all (< height) (treesNorth x y)
            || all (< height) (treesWest x y)
            || all (< height) (treesEast x y)
            || all (< height) (treesSouth x y)

    countOuterRim = 2 * (colTo - colFrom) + 2 * (rowTo - rowFrom)

    allTrees = do
      x <- [rowFrom .. rowTo]
      y <- [colFrom .. colTo]
      pure (x, y)

    innerSquare = do
      x <- [minX .. maxX]
      y <- [minY .. maxY]
      pure (x, y)

    -- Find all trees to the north
    treesNorth x _ | x == minX - 1 = []
    treesNorth x y = do
      x' <- [minX - 1 .. x - 1]
      pure (treeAtPos x' y)

    -- Find all trees to the south
    treesSouth x _ | x == maxX + 1 = []
    treesSouth x y = do
      x' <- [x + 1 .. maxX + 1]
      pure (treeAtPos x' y)

    -- Find all trees to the west
    treesWest _ y | y == minY - 1 = []
    treesWest x y = do
      y' <- [minY - 1 .. y - 1]
      pure (treeAtPos x y')

    -- Find all trees to the east
    treesEast _ y | y == maxY + 1 = []
    treesEast x y = do
      y' <- [y + 1 .. maxY + 1]
      pure (treeAtPos x y')

    -- Tree at position
    treeAtPos x y = (Array.!) (forest Array.! x) y

    -- Bounds for accessing the inner trees
    (rowFrom, rowTo) = Array.bounds forest
    (minX, maxX) = (rowFrom + 1, rowTo - 1)
    (minY, maxY) = (colFrom + 1, colTo - 1)

buildArrays :: [T.Text] -> (InnerBounds, Forest)
buildArrays ls = (innerBounds, rows)
  where
    rowSize = length ls
    colSize = T.length $ head ls
    innerBounds = (0, colSize - 1)
    rows = Array.listArray innerBounds cols
    cols = Array.listArray (0, rowSize - 1) . charsToInts . T.unpack <$> ls
    charsToInts = fmap (read . singleton)

readInput :: IO [T.Text]
readInput = T.lines <$> TIO.readFile "./inputs/day202208.txt"