{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.State.Strict (State, execState, get, put)
import Data.ByteString.Char8 (ByteString, lines, readFile, readInt, split)
import Data.HashMap.Strict (HashMap)
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
        parseSplit (from : "to" : to : "=" : distance : []) = (from, to, dist)
          where
            dist = case readInt distance of
              Just (d, _) -> d
              Nothing -> error "bad integer"
        parseSplit _ = error "bad line"

        collect :: (ByteString, ByteString, Int) -> State Graph ()
        collect (from, to, dist) = do
          g <- get
          let v = vertices g
              e = edges g
          put $
            Graph
              { edges = H.insert (from, to) dist $ H.insert (to, from) dist e,
                vertices = S.insert from $ S.insert to v
              }

cost :: Edges -> [ByteString] -> Int
cost costs (from : to : rest) = c (from, to) costs + cost costs (to : rest)
  where
    c = H.lookupDefault (error "bad city")
cost _ _ = 0

part1 :: ByteString -> Int
part1 input =
  let g = parse input
      cities = S.toList $ vertices g
      costs = edges g
      candidates = permutations cities
   in minimum $ map (cost costs) candidates

part2 :: ByteString -> Int
part2 input =
  let g = parse input
      cities = S.toList $ vertices g
      costs = edges g
      candidates = permutations cities
   in maximum $ map (cost costs) candidates

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ part1 input
  print $ part2 input
