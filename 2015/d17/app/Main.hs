module Main where

import Data.List

example :: [Int]
example = [20, 15, 10, 5, 5]

countFillings :: Int -> [Int] -> Int
countFillings 0 _ = 1
countFillings _ [] = 0
countFillings target available
  | target < 0 = 0
  | otherwise = sum $ map delve $ tails available
 where
  delve :: [Int] -> Int
  delve (choice : rest) = countFillings (target - choice) rest
  delve [] = 0

computeFillings :: Int -> [Int] -> [Int] -> [[Int]]
computeFillings 0 _ acc = [acc]
computeFillings _ [] _ = []
computeFillings target choices acc
  | target < 0 = []
  | otherwise = concat $ map delve $ tails choices
 where
  delve :: [Int] -> [[Int]]
  delve (choice : rest) = computeFillings (target - choice) rest (choice : acc)
  delve [] = []

part1 :: String -> Int
part1 input = countFillings 150 (map read $ lines input)

part2 :: String -> Int
part2 input =
  let
    parsed = (map read $ lines input)
    fillings = computeFillings 150 parsed []
    counts = map length fillings
    m = minimum counts
   in
    length $ filter (m ==) counts

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ part1 input
  print $ part2 input
