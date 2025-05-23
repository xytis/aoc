{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Either.Extra (eitherToMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Sue = Sue
  { sId :: Int
  , sChildren :: Maybe Int
  , sCats :: Maybe Int
  , sSamoyeds :: Maybe Int
  , sPomeranians :: Maybe Int
  , sAkitas :: Maybe Int
  , sVizslas :: Maybe Int
  , sGoldfish :: Maybe Int
  , sTrees :: Maybe Int
  , sCars :: Maybe Int
  , sPerfumes :: Maybe Int
  }
  deriving stock (Show)

type Parser = Parsec Void String

example :: String
example = "Sue 10: trees: 2, children: 10, samoyeds: 10"

type Fact = (String, Int)

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

integer :: Parser Int
integer = lexeme L.decimal

fact :: Parser Fact
fact = do
  _ <- optional space
  name <- many letterChar
  _ <- char ':'
  _ <- sc
  num <- integer
  _ <- optional $ char ','
  pure (name, num)

parseSue :: Parser Sue
parseSue = do
  _ <- lexeme $ string "Sue"
  sId <- L.decimal
  _ <- char ':'
  facts <- Map.fromList <$> many fact
  let sChildren = Map.lookup "children" facts
      sCats = Map.lookup "cats" facts
      sSamoyeds = Map.lookup "samoyeds" facts
      sPomeranians = Map.lookup "pomeranians" facts
      sAkitas = Map.lookup "akitas" facts
      sVizslas = Map.lookup "vizslas" facts
      sGoldfish = Map.lookup "goldfish" facts
      sTrees = Map.lookup "trees" facts
      sCars = Map.lookup "cars" facts
      sPerfumes = Map.lookup "perfumes" facts

  return Sue{..}

part1 :: String -> Int
part1 input =
  let sues = catMaybes $ map (eitherToMaybe . runParser parseSue "(input)") $ lines input
      incorrect :: Sue -> Bool
      incorrect sue =
        3 == (fromMaybe 3 $ sChildren sue)
          && 7 == (fromMaybe 7 $ sCats sue)
          && 2 == (fromMaybe 2 $ sSamoyeds sue)
          && 3 == (fromMaybe 3 $ sPomeranians sue)
          && 0 == (fromMaybe 0 $ sAkitas sue)
          && 0 == (fromMaybe 0 $ sVizslas sue)
          && 5 == (fromMaybe 5 $ sGoldfish sue)
          && 3 == (fromMaybe 3 $ sTrees sue)
          && 2 == (fromMaybe 2 $ sCars sue)
          && 1 == (fromMaybe 1 $ sPerfumes sue)
   in sId $ head $ filter incorrect sues

part2 :: String -> Int
part2 input =
  let sues = catMaybes $ map (eitherToMaybe . runParser parseSue "(input)") $ lines input
      incorrect :: Sue -> Bool
      incorrect sue =
        3 == (fromMaybe 3 $ sChildren sue)
          && 7 < (fromMaybe 8 $ sCats sue)
          && 2 == (fromMaybe 2 $ sSamoyeds sue)
          && 3 > (fromMaybe 2 $ sPomeranians sue)
          && 0 == (fromMaybe 0 $ sAkitas sue)
          && 0 == (fromMaybe 0 $ sVizslas sue)
          && 5 > (fromMaybe 4 $ sGoldfish sue)
          && 3 < (fromMaybe 4 $ sTrees sue)
          && 2 == (fromMaybe 2 $ sCars sue)
          && 1 == (fromMaybe 1 $ sPerfumes sue)
   in sId $ head $ filter incorrect sues

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ part1 input
  print $ part2 input
