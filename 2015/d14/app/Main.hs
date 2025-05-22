{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.List (sortBy)
import Data.Maybe (fromJust)
import Text.Scanf

example :: String
example = "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds."

type Name = String

type Speed = Int

type FlyTime = Int

type RestTime = Int

data Deer = Deer Name Speed FlyTime RestTime
  deriving stock (Show)

type Score = Int

type Distance = Int

data State = Fly FlyTime | Rest RestTime
  deriving stock (Show)

parse :: String -> Maybe Deer
parse str = do
  (name :+ speed :+ ft :+ rt :+ ()) <- scanf [fmt|%s can fly %d km/s for %d seconds, but then must rest for %d seconds.|] str :: Maybe (Name :+ Speed :+ FlyTime :+ RestTime :+ ())
  pure (Deer name speed ft rt)

distance :: Int -> Deer -> Int
distance time (Deer _ speed ft rt) =
  let period = ft + rt
      cycles = time `div` period
      remainder = time `mod` period
      lastf = min ft remainder
      dist = ft * cycles * speed + lastf * speed
   in dist

part1 :: String -> Int
part1 input =
  let deers = map parse $ lines input
      distances = map (distance 2503 . fromJust) deers
   in maximum distances

tick :: Deer -> (State, Distance) -> (State, Distance)
tick deer@(Deer _ v ft _) ((Fly t), d)
  | t == ft = tick deer (Rest 0, d)
  | otherwise = (Fly (t + 1), (d + v))
tick deer@(Deer _ _ _ rt) ((Rest t), d)
  | t == rt = tick deer (Fly 0, d)
  | otherwise = (Rest (t + 1), d)

tickAll :: [(Deer, (State, Distance), Score)] -> [(Deer, (State, Distance), Score)]
tickAll prev =
  let next = map (\(de, (st, di), sc) -> (de, tick de (st, di), sc)) prev
      sorted = sortBy (\(_, (_, d1), _) (_, (_, d2), _) -> compare d2 d1) next
      (_, (_, m), _) = head sorted
   in map (\(de, (st, di), sc) -> if m == di then (de, (st, di), sc + 1) else (de, (st, di), sc)) sorted

part2 :: String -> Int
part2 input =
  let deers = map parse $ lines input
      start = map (\d -> ((fromJust d), ((Fly 0), 0), 0)) deers
      finish = head $ drop 2503 $ iterate tickAll start
   in maximum $ map (\(_, _, s) -> s) finish

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ part1 input
  print $ part2 input
