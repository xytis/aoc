module D03 where

import Text.Parsec.String (Parser)
import Text.Parsec.Combinator (choice, many1)
import Text.Parsec.Char (char)
import Text.Parsec.Prim (parse)
import Data.Either.Unwrap (fromRight)
import Linear.V2
import Data.Set (Set, insert, union, size, fromList)

data Dir = DirUp | DirRight | DirDown | DirLeft
  deriving (Show)

part1 :: String -> Int
part1 input = size $ houses input

part2 :: String -> Int
part2 input = size $ union santa robot
  where santa = houses $ odds input
        robot = houses $ evens input

houses :: String -> Set (V2 Int)
houses input = fromList pack
  where pack = scanl move (V2 0 0) diffs
        diffs = map dirToDiff dirs
        dirs = inputToDirs input

evens (x:xs) = x:odds xs
evens _ = []

odds (_:xs) = evens xs
odds _ = []

move :: (Num a) => V2 a -> V2 a -> V2 a
move at diff = at + diff

aggr :: (Num a, Ord a) => (a, Set a) -> a -> (a, Set a)
aggr (c, s) d = (n, insert n s)
  where n = c + d

inputToDirs :: String -> [Dir]
inputToDirs = fromRight . parse (many1 dirParser) ""

dirToDiff :: Dir -> V2 Int
dirToDiff DirUp = V2 0 (-1)
dirToDiff DirRight = V2 1 0
dirToDiff DirDown = V2 0 1
dirToDiff DirLeft = V2 (-1) 0

dirParser :: Parser Dir
dirParser = c <$> choice [up,right,down,left]
  where up = char '^'
        right = char '>'
        down = char 'v'
        left = char '<'
        c :: Char -> Dir
        c '^' = DirUp
        c '>' = DirRight
        c 'v' = DirDown
        c '<' = DirLeft
        c _ = undefined
