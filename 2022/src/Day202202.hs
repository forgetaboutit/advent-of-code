module Day202202 (run) where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO

run :: IO ()
run = do
  input <- readInput

  let scoreMoves = parseAndEval parseResponseMove input
  putStrLn $ "Total score part 1: " ++ show scoreMoves

  let scoreOutcomes = parseAndEval parseOutcome input
  putStrLn $ "Total score part 2: " ++ show scoreOutcomes
  where
    parseAndEval parser input = sum $ evaluateRowScore <$> parseInput parser input

evaluateRowScore :: EvalGame a => (Move, a) -> Int
evaluateRowScore = uncurry eval

moveScore :: Move -> Int
moveScore Rock = 1
moveScore Paper = 2
moveScore Scissors = 3

roundScore :: Move -> Move -> Int
roundScore Rock Paper = 6
roundScore Paper Scissors = 6
roundScore Scissors Rock = 6
roundScore x y | x == y = 3
roundScore _ _ = 0

class EvalGame op where
  eval :: Move -> op -> Int

instance EvalGame Outcome where
  eval :: Move -> Outcome -> Int
  eval opMove outcome = moveScore ownMove + roundScore opMove ownMove
    where
      ownMove = case (opMove, outcome) of
        (move, Draw) -> move
        (Rock, Win) -> Paper
        (Rock, Loss) -> Scissors
        (Paper, Win) -> Scissors
        (Paper, Loss) -> Rock
        (Scissors, Win) -> Rock
        (Scissors, Loss) -> Paper

instance EvalGame Move where
  eval :: Move -> Move -> Int
  eval opMove ownMove = moveScore ownMove + roundScore opMove ownMove

parseInput :: (T.Text -> a) -> T.Text -> [(Move, a)]
parseInput parseSnd input = parseInputRow parseSnd <$> T.splitOn "\n" input

parseInputRow :: (T.Text -> a) -> T.Text -> (Move, a)
parseInputRow parseSnd row = (firstMove, response)
  where
    parts = T.splitOn " " row
    firstMove = parseFirstMove $ head parts
    response = parseSnd $ parts !! 1

data Move
  = Rock
  | Paper
  | Scissors
  deriving (Show, Eq, Enum)

data Outcome
  = Loss
  | Draw
  | Win
  deriving (Show, Eq)

parseFirstMove :: T.Text -> Move
parseFirstMove "A" = Rock
parseFirstMove "B" = Paper
parseFirstMove "C" = Scissors
parseFirstMove unknown = error $ "Undefined move: '" ++ show unknown ++ "'"

parseResponseMove :: T.Text -> Move
parseResponseMove "X" = Rock
parseResponseMove "Y" = Paper
parseResponseMove "Z" = Scissors
parseResponseMove unknown = error $ "Undefined move: '" ++ show unknown ++ "'"

parseOutcome :: T.Text -> Outcome
parseOutcome "X" = Loss
parseOutcome "Y" = Draw
parseOutcome "Z" = Win
parseOutcome unknown = error $ "Undefined outcome: '" ++ show unknown ++ "'"

readInput :: IO T.Text
readInput = TIO.readFile "./inputs/day202202.txt"
