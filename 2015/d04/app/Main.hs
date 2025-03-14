module Main where

import MyLib (part1, part2)

main :: IO ()
main = do
  let input = "yzbqklnj"
  print $ "part 1: " ++ show (part1 input)
  print $ "part 2: " ++ show (part2 input)

