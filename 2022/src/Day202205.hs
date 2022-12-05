module Day202205 (run) where

import Data.Functor (($>))
import Data.Functor.Identity (Identity)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHC.Arr qualified as Array
import Text.Parsec qualified as Parsec

run :: IO ()
run = do
  input <- readInput
  let (stacks, instructions) = parseInput input
  let tops = topItemsByMoveOrder Single stacks instructions
  print tops
  let tops' = topItemsByMoveOrder Multiple stacks instructions
  print tops'

type Stacks = Array.Array Int [T.Text]

type Instruction = (Int, Int, Int)

type State = ([[T.Text]], [Instruction])

topItemsByMoveOrder :: StackMoveOrder -> [[T.Text]] -> [Instruction] -> T.Text
topItemsByMoveOrder order stacks instructions =
  topItems $ foldl (executeInstruction order) (toArray stacks) instructions

topItems :: Stacks -> T.Text
topItems s = T.concat $ Array.elems $ head <$> s

data StackMoveOrder
  = Single
  | Multiple

executeInstruction :: StackMoveOrder -> Stacks -> Instruction -> Stacks
executeInstruction order s (cnt, from, to) = replacePlaces
  where
    replacePlaces = Array.accum (const id) s [(from, newFrom), (to, newTo)]
    newTo = newToOrder (take cnt fromPlace) ++ toPlace
    newToOrder = case order of
      Single -> reverse
      Multiple -> id
    newFrom = drop cnt fromPlace
    fromPlace = index from
    toPlace = index to
    index idx =
      s Array.! idx

parseInput :: [T.Text] -> State
parseInput input = parseState
  where
    parseState =
      let (stacks, instructions) = break (== "") input
       in ( buildStacks $ parseStacks $ drop 1 $ reverse stacks,
            parseInstruction <$> drop 1 instructions
          )

toArray :: [e] -> Array.Array Int e
toArray lst = Array.listArray (1, length lst) lst

buildStacks :: [[T.Text]] -> [[T.Text]]
buildStacks = go $ repeat []
  where
    go outs [] = outs
    go outs [row] = prepend <$> zip outs row
    go outs (row : rest) = go (prepend <$> zip outs row) rest
    prepend (xs, "") = xs
    prepend (xs, x) = x : xs

parseStacks :: [T.Text] -> [[T.Text]]
parseStacks = fmap $ parseOrFail parser
  where
    parser = Parsec.many1 $ parseItem <* itemSeparator
    itemSeparator = text " " Parsec.<|> Parsec.eof $> ""
    parseItem = parseSingleItem Parsec.<|> parseEmptyItem
    parseEmptyItem = text "   " $> ""
    parseSingleItem = text "[" *> (T.singleton <$> Parsec.letter) <* text "]"

parseInstruction :: T.Text -> Instruction
parseInstruction = parseOrFail parser
  where
    parser =
      (,,)
        <$> parsePrefixedInt "move "
        <*> parsePrefixedInt " from "
        <*> parsePrefixedInt " to "
    parsePrefixedInt prefix = text prefix *> parseInt
    parseInt = read <$> Parsec.many1 Parsec.digit

parseOrFail :: Parsec.ParsecT T.Text () Identity a -> T.Text -> a
parseOrFail parser input = case Parsec.runParser parser () "" input of
  Left e -> error $ "Parse error" ++ show e
  Right t -> t

text :: T.Text -> Parsec.ParsecT T.Text () Identity T.Text
text t = T.pack <$> Parsec.string (T.unpack t)

readInput :: IO [T.Text]
readInput = T.lines <$> TIO.readFile "./inputs/day202205.txt"