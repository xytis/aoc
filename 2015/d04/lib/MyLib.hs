module MyLib where

import Data.ByteString.UTF8 as BSU
import Data.ByteString.Base16 as BS16
import qualified Crypto.Hash.MD5 as MD5
import Data.List (isPrefixOf, find)
import Data.Maybe (fromJust)

sha :: String -> Int -> String
sha prefix number = BSU.toString (BS16.encode $ MD5.hash (BSU.fromString (prefix ++ (show number))))

fiveZeroes :: String -> Bool
fiveZeroes = isPrefixOf "00000"

sixZeroes :: String -> Bool
sixZeroes = isPrefixOf "000000"

part1 :: String -> Int
part1 prefix = fromJust $ find (\n -> fiveZeroes (sha prefix n)) [1..]

part2 :: String -> Int
part2 prefix = fromJust $ find (\n -> sixZeroes (sha prefix n)) [1..]
