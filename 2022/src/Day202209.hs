module Day202209 (run) where

import Data.Functor (($>))
import Data.Functor.Identity (Identity)
import Data.HashSet qualified as HS
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Text.Parsec qualified as Parsec

run :: IO ()
run = do
  input <- readInput
  let motions = parseInput input >>= splitMotion
  let finalState = foldl (flip evalMotion) (initialSimState 1) motions
  print $ HS.size $ visited finalState
  let finalState' = foldl (flip evalMotion) (initialSimState 9) motions
  print $ HS.size $ visited finalState'

evalMotion :: Direction -> SimState -> SimState
evalMotion m s =
  SimState
    { headPos = newHeadPos,
      followerPos = newFollowerPositions,
      visited = newVisited
    }
  where
    newHeadPos = applyMotion m $ headPos s
    newFollowerPositions = findNewFollowerPositions newHeadPos $ followerPos s
    newVisited = HS.insert (last newFollowerPositions) $ visited s

findNewFollowerPositions :: Position -> [Position] -> [Position]
findNewFollowerPositions newHeadPos followers =
  snd $ foldl findNewFollowerPos (newHeadPos, []) followers

findNewFollowerPos :: (Position, [Position]) -> Position -> (Position, [Position])
findNewFollowerPos (leader, acc) follower =
  let newPos = followLeader leader follower
   in (newPos, acc ++ [newPos])

data ReqMove
  = X
  | Y
  | Diag
  | None
  deriving (Show, Eq)

followLeader :: Position -> Position -> Position
followLeader (hx, hy) oldTailPos@(tx, ty) = newTailPos'
  where
    newTailPos' = case (deltaX, deltaY) of
      (2, 2) -> (tx + 1, ty + 1)
      (-2, 2) -> (tx - 1, ty + 1)
      (2, -2) -> (tx + 1, ty - 1)
      (-2, -2) -> (tx - 1, ty - 1)
      (2, 1) -> (tx + 1, ty + 1)
      (1, 2) -> (tx + 1, ty + 1)
      (-2, 1) -> (tx - 1, ty + 1)
      (-1, 2) -> (tx - 1, ty + 1)
      (2, -1) -> (tx + 1, ty - 1)
      (1, -2) -> (tx + 1, ty - 1)
      (-2, -1) -> (tx - 1, ty - 1)
      (-1, -2) -> (tx - 1, ty - 1)
      (2, _) -> (tx + 1, ty)
      (-2, _) -> (tx - 1, ty)
      (_, 2) -> (tx, ty + 1)
      (_, -2) -> (tx, ty - 1)
      _ -> oldTailPos
    deltaX = hx - tx
    deltaY = hy - ty

applyMotion :: Direction -> Position -> Position
applyMotion U (x, y) = (x, y + 1)
applyMotion D (x, y) = (x, y - 1)
applyMotion L (x, y) = (x - 1, y)
applyMotion R (x, y) = (x + 1, y)

splitMotion :: Motion -> [Direction]
splitMotion (Motion dir steps) = replicate steps dir

initialSimState :: Int -> SimState
initialSimState followers =
  SimState
    { headPos = (0, 0),
      followerPos = replicate followers (0, 0),
      visited = HS.empty
    }

type Position = (Int, Int)

data SimState = SimState
  { headPos :: Position,
    followerPos :: [Position],
    visited :: HS.HashSet Position
  }
  deriving (Show)

data Direction
  = U
  | D
  | L
  | R
  deriving (Show, Eq)

type Steps = Int

data Motion
  = Motion Direction Steps
  deriving (Show, Eq)

parseInput :: T.Text -> [Motion]
parseInput = parseOrFail parser
  where
    parser = Parsec.many parseMotion <* Parsec.eof
    parseMotion = Motion <$> parseDirection <*> parseSteps
    parseDirection =
      ( parseText "U" $> U
          Parsec.<|> parseText "D" $> D
          Parsec.<|> parseText "L" $> L
          Parsec.<|> parseText "R" $> R
      )
        <* parseText " "
    parseSteps = parseNum <* lineSep
    lineSep = newline Parsec.<|> Parsec.eof
    newline = Parsec.char '\n' $> ()
    parseNum = read <$> Parsec.many1 Parsec.digit

parseText :: T.Text -> Parsec.ParsecT T.Text () Identity T.Text
parseText t = T.pack <$> Parsec.string (T.unpack t)

parseOrFail :: Parsec.ParsecT T.Text () Identity a -> T.Text -> a
parseOrFail parser input = case Parsec.runParser parser () "" input of
  Left e -> error $ "Parse error" ++ show e
  Right t -> t

readInput :: IO T.Text
readInput = TIO.readFile "./inputs/day202209.txt"