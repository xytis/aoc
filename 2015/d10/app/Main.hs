module Main where

import Data.Char (isDigit)
import Data.List (group)

looksay :: [Char] -> [Char]
looksay = concat . (map say) . group
  where
    say :: [Char] -> [Char]
    say c = (show $ length c) <> (head c : [])

count :: Int -> String -> Int
count n input = length $ head $ drop n $ (iterate looksay) $ takeWhile isDigit input

part1 :: String -> Int
part1 = count 40

part2 :: String -> Int
part2 = count 50

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ part1 input
  print $ part2 input
