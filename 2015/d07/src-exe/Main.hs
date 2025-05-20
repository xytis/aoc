{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy
import Data.Bits (complement, shiftL, shiftR, (.&.), (.|.))
import Data.Either
import Data.HashMap.Lazy as H
import Data.Maybe (fromMaybe)
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as C
import Text.Parsec.String (Parser)

type Wire = String

data Val
  = Signal Int
  | Wire Wire
  deriving stock (Show)

data Expr
  = Val Val
  | And Val Val
  | Or Val Val
  | Not Val
  | LShift Val Int
  | RShift Val Int
  deriving stock (Show)

testExpr :: String
testExpr =
  unlines $
    "1113 -> x"
      : "1 -> y"
      : "x OR y -> z"
      : "x AND y -> z1"
      : "1 OR 2 -> c"
      : "NOT x -> z2"
      : "x LSHIFT 1 -> z3"
      : "x RSHIFT 2 -> z4"
      : []

stringP :: String -> Parser String
stringP = C.string

whitespaceP :: Parser ()
whitespaceP = void $ P.many $ C.oneOf " \t"

lexemeP :: Parser a -> Parser a
lexemeP p = do
  x <- p
  whitespaceP
  return x

labelP :: Parser String
labelP = P.many1 C.alphaNum

wireP :: Parser Val
wireP = Wire <$> lexemeP labelP

signalP :: Parser Val
signalP = Signal <$> lexemeP numP

valP :: Parser Val
valP = P.choice [P.try signalP, wireP]

opP :: String -> Parser ()
opP op = void $ lexemeP (stringP op)

numP :: Parser Int
numP = do
  n <- P.many1 C.digit
  return (read n)

binP :: (Val -> Val -> Expr) -> String -> Parser Expr
binP c op = do
  v1 <- valP
  _ <- opP op
  v2 <- valP
  return $ c v1 v2

orP :: Parser Expr
orP = binP Or "OR"

andP :: Parser Expr
andP = binP And "AND"

notP :: Parser Expr
notP = do
  _ <- opP "NOT"
  v <- valP
  return $ Not v

uncP :: (Val -> Int -> Expr) -> String -> Parser Expr
uncP c op = do
  v <- valP
  _ <- opP op
  n <- lexemeP $ numP
  return $ c v n

lshiftP :: Parser Expr
lshiftP = uncP LShift "LSHIFT"

rshiftP :: Parser Expr
rshiftP = uncP RShift "RSHIFT"

constP :: Parser Expr
constP = Val <$> valP

exprParser :: Parser (Wire, Expr)
exprParser = do
  let exprP =
        P.choice $
          P.try orP
            : P.try andP
            : P.try notP
            : P.try lshiftP
            : P.try rshiftP
            : P.try constP
            : []

  e <- exprP
  _ <- lexemeP $ stringP "->"
  w <- lexemeP labelP
  return (w, e)

inputParser :: Parser [(Wire, Expr)]
inputParser = P.endBy exprParser P.newline

parse :: String -> [(Wire, Expr)]
parse input = fromRight [] (P.parse inputParser "(input)" input)

part1 :: String -> Maybe Int
part1 input =
  let m = H.fromList $ parse input
   in evalStateT (resolve "a" m) H.empty

part2 :: String -> Maybe Int
part2 input =
  let m = H.fromList $ parse input
      p1 = fromMaybe 0 $ evalStateT (resolve "a" m) H.empty
      m' = H.insert "b" (Val (Signal p1)) m
   in evalStateT (resolve "a" m') H.empty

check :: String -> String
check input =
  let m = H.fromList $ parse input
   in show $ H.lookup "a" m

type Cache = H.HashMap Wire Int

type Fat a = StateT Cache Maybe a

ret :: Val -> H.HashMap Wire Expr -> Fat Int
ret (Wire w) m = resolve w m
ret (Signal s) _ = pure s

resolve :: Wire -> H.HashMap Wire Expr -> Fat Int
resolve w m = do
  c <- get
  n <- case H.lookup w c of
    Nothing -> case H.lookup w m of
      Just (Val v) -> ret v m
      Just (And e1 e2) -> (.&.) <$> ret e1 m <*> ret e2 m
      Just (Or e1 e2) -> (.|.) <$> ret e1 m <*> ret e2 m
      Just (Not e) -> complement <$> ret e m
      Just (LShift e n) -> shiftL <$> ret e m <*> pure n
      Just (RShift e n) -> shiftR <$> ret e m <*> pure n
      _ -> lift Nothing
    Just n -> pure n
  -- State changes in previous block
  c' <- get
  put $ H.insert w n c'
  return n

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ part1 input
  print $ part2 input
