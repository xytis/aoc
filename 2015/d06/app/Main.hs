{-# LANGUAGE GADTs, StandaloneDeriving, TypeSynonymInstances, FlexibleInstances, DerivingStrategies #-}

module Main where

import Data.Array
import Data.Array.MArray
import Data.Array.ST
import qualified Text.Parsec as Parsec
import Control.Monad.ST
import Prelude hiding (flip)


example :: String
example = "turn on 0,0 through 1,1\ntoggle 0,0 through 3,3\nturn off 2,2 through 3,3\n"

attempt :: Parsec.Parsec String () a -> Either Parsec.ParseError a
attempt rule = Parsec.parse rule "(example)" example

data Action = On | Off | Toggle
  deriving stock (Show, Eq)

data Range = Range (Int, Int) (Int, Int)
  deriving stock (Show, Eq)

data Operation = Operation Action Range
  deriving stock (Show, Eq)

operationParser :: Parsec.Parsec String () Operation
operationParser = do
  let
    on = On <$ Parsec.try (Parsec.string "turn on")
    off = Off <$ Parsec.try (Parsec.string "turn off")
    toggle = Toggle <$ Parsec.try (Parsec.string "toggle")
    pair = do
      x <- read <$> Parsec.many1 Parsec.digit
      _ <- Parsec.char ','
      y <- read <$> Parsec.many1 Parsec.digit
      pure (x, y)

  action <- Parsec.choice [on, off, toggle]
  _ <- Parsec.space
  (x1, y1) <- pair
  _ <- Parsec.string " through "
  (x2, y2) <- pair
  pure $ Operation action $ Range (x1, y1) (x2, y2)

inputParser :: Parsec.Parsec   String   ()   [Operation]
inputParser = Parsec.endBy operationParser Parsec.newline


-- >>> attempt inputParser

part1 :: String -> Int
part1 input = 
  let 
    ops = case Parsec.parse inputParser "(part 1)" input of
      Right o ->  o
      Left e -> error (show e)
    grid = runSTArray $ do
      arr <- newArray ((0,0), (999,999)) False :: ST s (STGrid s Bool)
      mapM_ (mutBool arr) ops 
      return arr
  in foldl' (\acc el -> if el then acc + 1 else acc) 0 grid

part2 :: String -> Int
part2 input =
  let
    ops = case Parsec.parse inputParser "(part 1)" input of
      Right o ->  o
      Left e -> error (show e)
    grid = runSTArray $ do
      arr <- newArray ((0,0), (999,999)) 0 :: ST s (STGrid s Int)
      mapM_ (mutInt arr) ops 
      return arr
  in foldl' (+) 0 grid


type Coords = (Int, Int)
type Grid a = Array Coords a

type STGrid s a = STArray s Coords a

actBool :: Action -> STGrid s Bool -> Coords -> ST s ()
actBool op arr at = do
  case op of
    On -> do
      writeArray arr at True
    Off -> do
      writeArray arr at False
    Toggle -> do
      modifyArray arr at (not)

actInt :: Action -> STGrid s Int -> Coords -> ST s ()
actInt op arr at = do
  case op of
    On -> do
      modifyArray arr at (+1)
    Off -> do
      modifyArray arr at (\v -> max (v-1) 0)
    Toggle -> do
      modifyArray arr at (+2)


mutBool :: STGrid s Bool -> Operation -> ST s ()
mutBool arr (Operation op (Range from to)) = do
  mapM_ (actBool op arr) $ range (from, to)

mutInt :: STGrid s Int -> Operation -> ST s ()
mutInt arr (Operation op (Range from to)) = do
  mapM_ (actInt op arr) $ range (from, to)

test :: [Operation] -> Grid Bool
test ops = runSTArray $ do
  arr <- newArray ((0,0), (3,3)) False :: ST s (STGrid s Bool)
  mapM_ (mutBool arr) ops 
  return arr


main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ "part 1: " ++ show (part1 input)
  print $ "part 2: " ++ show (part2 input)

