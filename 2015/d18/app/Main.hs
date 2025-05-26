{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Monad (guard)
import Control.Monad.ST

import Data.Array.MArray
import Data.Array.ST
import Data.Array.Unboxed
import Debug.Trace

between :: Int -> Int -> Int -> Bool
between l h d = (l <= d) && (d <= h)

minC :: Int
minC = 0

maxC :: Int
maxC = 99

minCC :: (Int, Int)
minCC = (minC, minC)

maxCC :: (Int, Int)
maxCC = (maxC, maxC)

rngCC :: ((Int, Int), (Int, Int))
rngCC = (minCC, maxCC)

inBounds :: (Int, Int) -> Bool
inBounds (x, y) = (between minC maxC x) && (between minC maxC y)

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x0, y0) = do
  x <- [(x0 - 1) .. (x0 + 1)]
  y <- [(y0 - 1) .. (y0 + 1)]
  guard $ (x, y) /= (x0, y0)
  guard $ inBounds (x, y)
  return (x, y)

type V2 = (Int, Int)
type Grid s = STUArray s V2 Bool

update :: (Grid s, Grid s) -> V2 -> ST s ()
update (prev, next) at = do
  cell <- readArray prev at
  around <- mapM (readArray prev) (neighbors at)
  let
    friends = sum $ map alive $ around
    value = case cell of
      True -> case friends of
        2 -> True
        3 -> True
        _ -> False
      False -> case friends of
        3 -> True
        _ -> False
  writeArray next at value

alive :: Bool -> Int
alive False = 0
alive True = 1

tick :: (Grid s, Grid s) -> ST s (Grid s, Grid s)
tick (prev, next) = do
  bnd <- getBounds prev
  mapM_ (update (prev, next)) (range bnd)
  pure (next, prev)

tick' :: (Grid s, Grid s) -> ST s (Grid s, Grid s)
tick' (prev, next) = do
  bnd <- getBounds prev
  mapM_ (update (prev, next)) $ range bnd
  writeArray next (0, 0) True
  writeArray next (0, 99) True
  writeArray next (99, 0) True
  writeArray next (99, 99) True
  pure (next, prev)

iterateNM :: (Monad m) => Int -> (a -> m a) -> m a -> m [a]
iterateNM 0 _ _ = return []
iterateNM k step start = do
  first <- start
  rest <- iterateNM (k - 1) step (step first)
  return (first : rest)

executeN :: (Monad m) => Int -> (a -> m a) -> m a -> m a
executeN 0 _ start = start
executeN k step start = do
  prev <- start
  executeN (k - 1) step (step prev)

fillGrid :: Grid s -> String -> ST s ()
fillGrid arr input = do
  let
    toElement :: Int -> Int -> Char -> ((Int, Int), Bool)
    toElement x y e = ((x, y), t)
     where
      t = case e of
        '#' -> True
        _ -> False
    rows :: [(Int, String)]
    rows = zip [0 ..] (lines input)
    parseRow :: (Int, String) -> [((Int, Int), Bool)]
    parseRow (y, s) = map (\(x, e) -> toElement x y e) (zip [0 ..] s)
    els = concat $ map parseRow rows

  mapM_ (\(i, e) -> writeArray arr i e) els

part1 :: Int -> String -> Int
part1 steps input =
  let
    final :: UArray V2 Bool
    final = runSTUArray $ do
      arr1 <- newArray_ rngCC
      arr2 <- newArray_ rngCC
      fillGrid arr1 input
      (res, _) <- executeN steps tick $ pure (arr1, arr2)
      pure res
   in
    sumShining final

sumShining :: UArray V2 Bool -> Int
sumShining = foldl' (\acc e -> if e then acc + 1 else acc) 0 . elems

part2 :: Int -> String -> Int
part2 steps input =
  let
    final :: UArray V2 Bool
    final = runSTUArray $ do
      arr1 <- newArray_ rngCC
      arr2 <- newArray_ rngCC
      fillGrid arr1 input
      writeArray arr1 (0, 0) True
      writeArray arr1 (0, 99) True
      writeArray arr1 (99, 0) True
      writeArray arr1 (99, 99) True
      (res, _) <- executeN steps tick' $ pure (arr1, arr2)
      pure res
   in
    sumShining final

printArray :: UArray V2 Bool -> String
printArray arr =
  unlines [unwords [conv (arr ! (x, y)) | x <- [x0 .. xm]] | y <- [y0 .. ym]]
 where
  conv True = "#"
  conv False = "."
  ((x0, y0), (xm, ym)) = bounds arr

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ part1 100 input
  print $ part2 100 input
