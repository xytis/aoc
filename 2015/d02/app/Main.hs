module Main where

import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Text.Parsec.Prim
import Data.Either.Unwrap

data Box = Box (Int, Int, Int)
  deriving (Show)

main :: IO ()
main = do
  boxes <- (map lineToBox) <$> lines <$> readFile "input.txt"
  let p1 = sum $ map paper boxes
  print $ "part1: " ++ show p1
  let p2 = sum $ map ribbon boxes
  print $ "part2: " ++ show p2


lineToBox :: String -> Box
lineToBox = fromRight . parse boxParser ""

num :: Parser Int
num = read <$> many1 digit

minl :: (Ord a) => [a] -> a
minl = foldr1 min

boxParser :: Parser Box
boxParser = ( Box ..: (,,) ) <$> (num <* x) <*> num <*> (x *> num)
  where x = char 'x'

paper :: Box -> Int
paper (Box (l, w, h)) = (minl sides) + 2 * (sum sides)
  where sides = l*w : w*h : h*l : []

ribbon :: Box -> Int
ribbon (Box (l, w, h)) = (minl perimeters) + volume
  where volume = l*w*h
        perimeters = 2*(l+w) : 2*(w+h) : 2*(l+h) : []

-- Blackbird
(.:) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(.:) = (.) . (.)

-- eeh?
(..:) :: (b -> c) -> (a1 -> a2 -> a3 -> b) -> a1 -> a2 -> a3 -> c
(..:) = (.:) . (.)
