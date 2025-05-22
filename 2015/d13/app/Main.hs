{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.State.Strict (State, execState, get, put)
import Data.ByteString.Char8 (ByteString, dropEnd, lines, readFile, readInt, split)
import Data.HashMap.Strict (HashMap, fromList, union)
import qualified Data.HashMap.Strict as H
import Data.List (permutations)
import Data.Set (Set)
import qualified Data.Set as S
import Prelude hiding (lines, readFile)

type Edge = (ByteString, ByteString)

type Edges = HashMap Edge Int

type Vertices = Set ByteString

data Graph = Graph
  { edges :: Edges,
    vertices :: Vertices
  }
  deriving stock (Show)

parse :: ByteString -> Graph
parse = run . lines
  where
    run :: [ByteString] -> Graph
    run entries = execState (extract entries) Graph {edges = H.empty, vertices = S.empty}

    extract :: [ByteString] -> State Graph ()
    extract = mapM_ $ collect . parseSplit . split ' '
      where
        parseSplit :: [ByteString] -> (ByteString, ByteString, Int)
        parseSplit (from : "would" : sign : value : "happiness" : "units" : "by" : "sitting" : "next" : "to" : toDot : []) = (from, to, val)
          where
            to = dropEnd 1 toDot
            val = case (readInt value, sign) of
              (Just (d, _), "gain") -> d
              (Just (d, _), "lose") -> (-d)
              (_, _) -> error "bad integer or sign"
        parseSplit _ = error "bad line"

        collect :: (ByteString, ByteString, Int) -> State Graph ()
        collect (from, to, dist) = do
          g <- get
          let v = vertices g
              e = edges g
          put $
            Graph
              { edges = H.insert (from, to) dist e,
                vertices = S.insert from $ S.insert to v
              }

happiness :: Edges -> [ByteString] -> Int
happiness costs (from : to : rest) = (c (from, to) costs + c (to, from) costs) + happiness costs (to : rest)
  where
    c = H.lookupDefault (error "bad guest name")
happiness _ _ = 0

mkCircle :: [a] -> [a]
mkCircle l@(f : _) = reverse (f : (reverse l))
mkCircle [] = []

part1 :: ByteString -> Int
part1 input =
  let g = parse input
      guests = S.toList $ vertices g
      costs = edges g
      candidates = map mkCircle $ permutations guests
   in maximum $ map (happiness costs) candidates

mySeating :: [ByteString] -> Edges
mySeating = fromList . aux []
  where
    aux :: [(Edge, Int)] -> [ByteString] -> [(Edge, Int)]
    aux es (g : gs) = aux ((("me", g), 0) : ((g, "me"), 0) : es) gs
    aux es [] = es

part2 :: ByteString -> Int
part2 input =
  let g = parse input
      guests = S.toList $ vertices g
      costs = (mySeating guests) `union` (edges g)
      guestsWithMe = ("me" : guests)
      candidates = map mkCircle $ permutations guestsWithMe
   in maximum $ map (happiness costs) candidates

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ part1 input
  print $ part2 input
