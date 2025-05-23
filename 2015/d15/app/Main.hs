{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Maybe (fromJust)
import Data.Tuple.Homogenous
import Text.Scanf

type Proportions = [Int]

type Spoons = Int

type Ingredients = Int

type Ingredient = Tuple5 Int

build :: Ingredients -> Spoons -> [Proportions]
build 0 _ = return []
build 1 m = do
  let x1 = m
  return $ x1 : []
build 2 m = do
  x1 <- [0 .. m]
  let x2 = m - x1
  return $ x1 : x2 : []
build 3 m = do
  x1 <- [0 .. m]
  x2 <- [0 .. m - x1]
  let x3 = m - x1 - x2
  return $ x1 : x2 : x3 : []
build 4 m = do
  x1 <- [0 .. m]
  x2 <- [0 .. m - x1]
  x3 <- [0 .. m - x1 - x2]
  let x4 = m - x1 - x2 - x3
  return $ x1 : x2 : x3 : x4 : []
build _ _ = undefined

score :: [Ingredient] -> Proportions -> Int
score ings props = prod . untuple5 $ foldl1 memberwise $ zipWith aux ings props
  where
    aux :: Tuple5 Int -> Int -> Tuple5 Int
    aux ing prop = (prop *) <$> ing
    memberwise :: Ingredient -> Ingredient -> Ingredient
    memberwise a b = (+) <$> a <*> b
    prod (a, b, c, d, _) = product $ (max 0) <$> [a, b, c, d]

score2 :: [Ingredient] -> Proportions -> Int
score2 ings props = prod . untuple5 $ foldl1 memberwise $ zipWith aux ings props
  where
    aux :: Tuple5 Int -> Int -> Tuple5 Int
    aux ing prop = (prop *) <$> ing
    memberwise :: Ingredient -> Ingredient -> Ingredient
    memberwise a b = (+) <$> a <*> b
    prod (a, b, c, d, s500) = if s500 == 500 then product $ (max 0) <$> [a, b, c, d] else 0

parse :: String -> Ingredient
parse str =
  let (_ :+ a :+ b :+ c :+ d :+ e :+ ()) = fromJust $ scanf [fmt|%s capacity %d, durability %d, flavor %d, texture %d, calories %d|] str
   in tuple5 a b c d e

part1 :: String -> Int
part1 input =
  let ings = map parse $ lines input
   in maximum $ map (score ings) (build 4 100)

part2 :: String -> Int
part2 input =
  let ings = map parse $ lines input
   in maximum $ map (score2 ings) (build 4 100)

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ part1 input
  print $ part2 input
