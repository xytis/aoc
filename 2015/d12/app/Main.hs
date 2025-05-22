module Main where

import Data.Char (isAlpha, isDigit)

packNums :: (Int, String) -> Char -> (Int, String)
packNums (s, trail) c
  | (isAlpha c) && (null trail) = (s, trail)
  | (isDigit c) || (c == '-') = (s, c : trail)
  | not $ null trail = (s + ((read $ reverse trail) :: Int), [])
  | otherwise = (s, trail)

part1 :: String -> Int
part1 input =
  let (s, trail) = foldl' packNums (0, "") input
      l = if (not $ null trail) then ((read $ reverse trail) :: Int) else 0
   in s + l

data Ctx = Obj | Arr

-- con drops current object tokens, direction based on first argument
con :: Char -> [Ctx] -> [Char] -> [Char]
con '}' [] ('}' : cs) = cs
con '}' (Obj : xs) ('}' : cs) = con '}' xs cs
con '}' (Arr : xs) (']' : cs) = con '}' xs cs
con '}' xs ('{' : cs) = con '}' (Obj : xs) cs
con '}' xs ('[' : cs) = con '}' (Arr : xs) cs
con '{' [] ('{' : cs) = cs
con '{' (Obj : xs) ('{' : cs) = con '{' xs cs
con '{' (Arr : xs) ('[' : cs) = con '{' xs cs
con '{' xs ('}' : cs) = con '{' (Obj : xs) cs
con '{' xs (']' : cs) = con '{' (Arr : xs) cs
con t xs (_ : cs) = con t xs cs
con _ _ [] = []

-- rednull replaces all objects with value "red" to null
rednull :: [Char] -> [Char] -> [Char]
rednull prev (':' : '"' : 'r' : 'e' : 'd' : '"' : next) =
  let nprev = con '{' [] prev
      nnext = con '}' [] next
   in rednull nprev ("null" <> nnext)
rednull prev (c : next) = rednull (c : prev) next
rednull prev [] = reverse prev

part2 :: String -> Int
part2 input = part1 $ rednull [] input

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ part1 input
  print $ part2 input
