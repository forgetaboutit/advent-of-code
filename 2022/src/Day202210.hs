module Day202210 (run) where

import Data.Functor (($>))
import Data.Functor.Identity (Identity)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Text.Parsec qualified as Parsec

run :: IO ()
run = do
  input <- readInput
  let instructions = parseInput input
  let executedStates = concatMap NE.toList (tickCycles execInstruction initialState instructions)
  print $ sum $ interestingSignalStrengths executedStates
  let crt = runSpriteWindow executedStates
  putStrLn $ draw crt

data Instruction
  = AddX Int
  | Noop
  deriving (Show, Eq)

data State = State
  { cycleNum :: Int,
    regXDuringCycle :: Int,
    regXAfterCycle :: Int
  }
  deriving (Show)

draw :: [Bool] -> String
draw pixels = drawBoard $ chunks 40 pixels
  where
    drawBoard rows = unlines $ drawRow <$> rows
    drawRow row = drawPixel <$> row
    drawPixel True = '#'
    drawPixel False = '.'

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks size xs =
  let (x, y) = splitAt size xs
   in x : chunks size y

runSpriteWindow :: [State] -> [Bool]
runSpriteWindow states =
  shouldDrawPixel <$> states
  where
    shouldDrawPixel state = case (spritePositions state, (cycleNum state - 1) `mod` 40) of
      ((x1, x2, x3), pixel) | x1 == pixel || x2 == pixel || x3 == pixel -> True
      _ -> False

    spritePositions state =
      let x = regXDuringCycle state
       in (x - 1, x, x + 1)

interestingSignalStrengths :: [State] -> [Int]
interestingSignalStrengths states = signalStrength <$> filter interestingIndex states
  where
    interestingIndex state = cycleNum state `elem` [20, 60, 100, 140, 180, 220]

signalStrength :: State -> Int
signalStrength state = cycleNum state * regXDuringCycle state

initialState :: State
initialState =
  State
    { cycleNum = 0,
      regXDuringCycle = 1,
      regXAfterCycle = 1
    }

tickCycles :: (t1 -> t2 -> NE.NonEmpty t1) -> t1 -> [t2] -> [NE.NonEmpty t1]
tickCycles _ _ [] = []
tickCycles f a xs = goTick f (NE.singleton a) xs
  where
    goTick :: (t1 -> t2 -> NE.NonEmpty t1) -> NE.NonEmpty t1 -> [t2] -> [NE.NonEmpty t1]
    goTick f' a' ls =
      case ls of
        [] -> []
        x' : xs' -> case f' (NE.last a') x' of
          res -> res : goTick f' res xs'

execInstruction :: State -> Instruction -> NE.NonEmpty State
execInstruction state instr = case instr of
  AddX v ->
    ( state
        { cycleNum = cycleNum state + 1,
          regXDuringCycle = regXAfterCycle state
        }
    )
      `NE.cons` NE.singleton
        ( state
            { cycleNum = cycleNum state + 2,
              regXDuringCycle = regXAfterCycle state,
              regXAfterCycle = regXAfterCycle state + v
            }
        )
  Noop ->
    NE.singleton $
      state
        { cycleNum = cycleNum state + 1,
          regXDuringCycle = regXAfterCycle state
        }

parseInput :: T.Text -> [Instruction]
parseInput = parseOrFail parser
  where
    parser = Parsec.many parseInstruction <* Parsec.eof
    parseInstruction = parseNoop Parsec.<|> parseAddX
    parseNoop = (parseText "noop" $> Noop) <* lineSep
    parseAddX = (AddX <$> (parseText "addx " *> parseInt)) <* lineSep
    lineSep = newline Parsec.<|> Parsec.eof
    newline = Parsec.char '\n' $> ()
    parseInt = do
      sign <- parseSign
      sign <$> parsePosNum
    parseSign = maybe (* 1) (const (* (-1))) <$> Parsec.optionMaybe (parseText "-")
    parsePosNum = read <$> Parsec.many1 Parsec.digit

parseText :: T.Text -> Parsec.ParsecT T.Text () Identity T.Text
parseText t = T.pack <$> Parsec.string (T.unpack t)

parseOrFail :: Parsec.ParsecT T.Text () Identity a -> T.Text -> a
parseOrFail parser input = case Parsec.runParser parser () "" input of
  Left e -> error $ "Parse error" ++ show e
  Right t -> t

readInput :: IO T.Text
readInput = TIO.readFile "./inputs/day202210.txt"