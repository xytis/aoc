module Main where

import Data.Char (isAlpha, isDigit)

packNums :: (Int, String) -> Char -> (Int, String)
packNums (s, trail) c
  | (isAlpha c) && (null trail) = (s, trail)
  | (isDigit c) || (c == '-') = (s, c : trail)
  | not $ null trail = (s + ((read $ reverse trail) :: Int), [])
  | otherwise = (s, trail)

part1 :: String -> Int
part1 input =
  let (s, trail) = foldl' packNums (0, "") input
      l = if (not $ null trail) then ((read $ reverse trail) :: Int) else 0
   in s + l

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ part1 input
