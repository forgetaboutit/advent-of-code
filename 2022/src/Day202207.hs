module Day202207 (run) where

import Data.Functor (($>))
import Data.Functor.Identity (Identity)
import Data.HashMap.Strict qualified as HM
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Text.Parsec qualified as Parsec

run :: IO ()
run = do
  input <- readInput
  let evalledFs = evalCmds $ parseInput input
  let tree = toTree evalledFs

  print $ totalSmallDirsSize' tree
  print $ smallestDeletableDirSize' tree

type Path = [T.Text]

type FsState = HM.HashMap Path [FileOrDir]

type FileSize = Int

data Tree a = Tree a [Tree a]
  deriving (Show, Eq)

type FsTree = Tree [FileSize]

foldFs :: (FsTree -> a -> a) -> a -> FsTree -> a
foldFs f a tree@(Tree _ leaves) = f tree $ foldr (flip (foldFs f)) a leaves

smallestDeletableDirSize' :: FsTree -> Maybe Int
smallestDeletableDirSize' tree = foldFs findFolder Nothing tree
  where
    findFolder t Nothing =
      let ourSize = treeSize t
       in if ourSize >= stillRequiredFreeSpace
            then Just ourSize
            else Nothing
    findFolder t (Just size) =
      let ourSize = treeSize t
       in if ourSize < size && ourSize >= stillRequiredFreeSpace
            then Just ourSize
            else Just size

    totalUsedSpace = treeSize tree
    requiredFreeSpace = 30000000
    totalFsSpace = 70000000
    freeSpace = totalFsSpace - totalUsedSpace
    stillRequiredFreeSpace = requiredFreeSpace - freeSpace

totalSmallDirsSize' :: FsTree -> Int
totalSmallDirsSize' = foldFs sizeFold 0
  where
    sizeFold tree a =
      let size = treeSize tree
       in a
            + if size <= 100000
              then size
              else 0

treeSize :: FsTree -> Int
treeSize (Tree fs dirs) =
  sum fs + sum (treeSize <$> dirs)

toTree :: FsState -> FsTree
toTree fsState = go (atKey [] fsState) []
  where
    go fs bc =
      let subDirs = findDirs fs
          subFiles = findFiles fs
          subTrees = map (\dir -> go (atKey (dir : bc) fsState) (dir : bc)) subDirs
       in Tree subFiles subTrees

    findDirs = mapMaybe matchDir
    findFiles = mapMaybe matchFile

    matchFile (File size _) = Just size
    matchFile _ = Nothing

    matchDir (Dir dirName) = Just dirName
    matchDir _ = Nothing

atKey :: Path -> FsState -> [FileOrDir]
atKey p fsState = case HM.lookup p fsState of
  Just fs -> fs
  Nothing -> error $ "Key " ++ show p ++ " is missing"

evalCmds :: [Command] -> FsState
evalCmds cmds = eval cmds HM.empty []
  where
    eval [] fsState _ = fsState
    eval (x : xs) fsState bc =
      let (fsState', bc') = evalCmd x fsState bc
       in eval xs fsState' bc'

    evalCmd :: Command -> FsState -> Path -> (FsState, Path)
    evalCmd (Cd Up) fsState bc = (fsState, tail bc)
    evalCmd (Cd (Down dir)) fsState bc = (fsState, dir : bc)
    evalCmd (Cd Root) fsState _ = (fsState, [])
    evalCmd (Ls fs) fsState bc =
      let fsState' = HM.insert bc fs fsState
       in (fsState', bc)

data Command
  = Cd Dest
  | Ls [FileOrDir]
  deriving (Show, Eq)

data Dest
  = Up
  | Down T.Text
  | Root
  deriving (Show, Eq)

data FileOrDir
  = File Int T.Text
  | Dir T.Text
  deriving (Show, Eq)

parseInput :: T.Text -> [Command]
parseInput = parseOrFail parser
  where
    parser = Parsec.many parseCmd <* Parsec.eof
    parseCmd = parseText "$ " *> (parseLs Parsec.<|> parseCd)
    parseCd = Cd <$> (parseText "cd " *> parseCdTarget <* lineSep)
    parseCdTarget =
      parseGoUp
        Parsec.<|> parseGoRoot
        Parsec.<|> parseGoDown
    parseGoUp = parseText ".." $> Up
    parseGoRoot = parseText "/" $> Root
    parseGoDown = Down <$> parseIdent
    parseLs = Ls <$> ((parseText "ls" <* newline) *> parseLsOutput)
    parseLsOutput = Parsec.many $ parseDir Parsec.<|> parseFile
    parseDir = Dir <$> (parseText "dir " *> parseIdent <* lineSep)
    parseFile = File <$> parseNum <*> (parseText " " *> parseIdent <* lineSep)
    lineSep = newline Parsec.<|> Parsec.eof
    newline = Parsec.char '\n' $> ()
    parseIdent = T.pack <$> Parsec.many1 (Parsec.letter Parsec.<|> Parsec.char '.')
    parseNum = read <$> Parsec.many1 Parsec.digit

parseText :: T.Text -> Parsec.ParsecT T.Text () Identity T.Text
parseText t = T.pack <$> Parsec.string (T.unpack t)

parseOrFail :: Parsec.ParsecT T.Text () Identity a -> T.Text -> a
parseOrFail parser input = case Parsec.runParser parser () "" input of
  Left e -> error $ "Parse error" ++ show e
  Right t -> t

readInput :: IO T.Text
readInput = TIO.readFile "./inputs/day202207.txt"