module Main where

import D03

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ "part 1: " ++ show (part1 input)
  print $ "part 2: " ++ show (part2 input)


