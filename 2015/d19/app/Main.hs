{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.List
import Data.List.Split
import Data.Maybe (fromMaybe)
import Text.Scanf

uniq :: (Eq b) => [b] -> [b]
uniq = map head . group

multiply :: String -> (String, String) -> [String]
multiply on (from, to) = go [] on
 where
  go :: String -> String -> [String]
  go _ [] = []
  go prev next@(x : xs) = case stripPrefix from next of
    Nothing -> go (x : prev) xs
    Just ws -> ((reverse prev) <> to <> ws) : go (x : prev) xs

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

explore :: String -> [(String, String)] -> [String]
explore on pairs =
  let
    m = map (multiply on) pairs
   in
    uniq $ sort $ concat m

examples :: [(String, String)]
examples =
  [ ("H", "HO")
  , ("H", "OH")
  , ("O", "HH")
  ]

examples2 =
  [ ("e", "H")
  , ("e", "O")
  , ("H", "HO")
  , ("H", "OH")
  , ("O", "HH")
  ]

parse :: String -> ([(String, String)], String)
parse input =
  let
    l = lines input
    parsePair :: String -> (String, String)
    parsePair str = fromMaybe undefined $ do
      (k :+ v :+ ()) <- scanf [fmt|%s => %s|] str :: Maybe (String :+ String :+ ())
      pure (k, v)
    parsePairs :: [String] -> [(String, String)]
    parsePairs = map parsePair

    [pairs, [on]] = splitOn [""] l
   in
    (parsePairs pairs, on)

parseElements :: String -> [String]
parseElements input =
  undefined

part1 :: String -> Int
part1 input =
  let
    (pairs, seed) = parse input
    candidates = explore seed pairs
   in
    length candidates

part2 :: String -> Int
part2 input =
  let
    (pairs, target) = parse input
    go iteration possible
      | target `elem` possible = iteration
      | otherwise =
          let
            next = uniq $ sort $ concat $ map (\s -> explore s pairs) possible
           in
            go (iteration + 1) next
   in
    go 0 ["e"]

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ part1 input
  print $ part2 input
