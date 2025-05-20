module Main where

import Control.Arrow ((>>>))
import Data.ByteString.Char8 (ByteString, cons, drop, dropEnd, length, lines, pack, readFile, snoc, unpack)
import Data.Char (chr)
import Prelude hiding (drop, length, lines, read, readFile)

unquote :: ByteString -> ByteString
unquote = (drop 1) . (dropEnd 1)

quote :: ByteString -> ByteString
quote s = ('"' `cons` s) `snoc` '"'

unencode :: ByteString -> ByteString
unencode = pack . unencode' . unpack

unencode' :: String -> String
unencode' ('\\' : 'x' : a : b : cs) = (chr (c a + c b)) : unencode' cs
unencode' ('\\' : '\\' : cs) = '\\' : unencode' cs
unencode' ('\\' : '"' : cs) = '"' : unencode' cs
unencode' (c' : cs) = c' : unencode' cs
unencode' [] = []

c :: Char -> Int
c '0' = 0
c '1' = 1
c '2' = 2
c '3' = 3
c '4' = 4
c '5' = 5
c '6' = 6
c '7' = 7
c '8' = 8
c '9' = 9
c 'A' = 10
c 'B' = 11
c 'C' = 12
c 'D' = 13
c 'E' = 14
c 'F' = 15
c 'a' = 10
c 'b' = 11
c 'c' = 12
c 'd' = 13
c 'e' = 14
c 'f' = 15
c _ = error "invalid hex digit"

encode :: ByteString -> ByteString
encode = pack . encode' . unpack

encode' :: String -> String
encode' ('"' : cs) = '\\' : '\"' : encode' cs
encode' ('\\' : cs) = '\\' : '\\' : encode' cs
encode' (c' : cs) = c' : encode' cs
encode' [] = []

part1line :: ByteString -> Int
part1line raw =
  let rawl = length raw
      norml = length $ unencode . unquote $ raw
   in rawl - norml

part1 :: ByteString -> Int
part1 = lines >>> map part1line >>> sum

part2line :: ByteString -> Int
part2line raw =
  let rawl = length raw
      encl = length $ quote . encode $ raw
   in encl - rawl

part2 :: ByteString -> Int
part2 = lines >>> map part2line >>> sum

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ part1 input
  print $ part2 input
