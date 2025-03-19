module Main where

import Data.List
import Data.Maybe

uniq :: Eq b => [b] -> [b]
uniq = map head . group

window :: [a] -> [[a]]
window [] = []
window [_] = []
window (a:b:[]) = [ (a:b:[]) ]
window (a:b:r) = (a:b:[]) : window (b:r)

window3 :: [a] -> [[a]]
window3 [] = []
window3 [_] = []
window3 [_,_] = []
window3 (a:b:c:[]) = [ (a:b:c:[]) ]
window3 (a:b:c:r) = (a:b:c:[]) : window3 (b:c:r)


nice :: String -> Bool
nice input = hasThreeVovels && hasALetterTwice && (not hasBadPairs)
  where hasThreeVovels = length (filter (\l -> l `elem` "aeiou") input) > 2
        hasALetterTwice = length (filter ((>1).length) (group input)) > 0
        hasBadPairs = isJust $ find (\p -> isJust $ find (p==) ["ab", "cd", "pq", "xy"] ) (window input)

nicer :: String -> Bool
nicer input = hasPairs && hasSandwitch 
  where
    hasPairs = offsetPairs (window input)
      where 
        offsetPairs (x:y:r) = elem x r || offsetPairs (y:r)
        offsetPairs _ = False

    hasSandwitch = any isSandwich (window3 input)
      where
        isSandwich (x:_:z:[])
          | x == z = True
          | otherwise = False
        isSandwich _ = False

part1 :: String -> Int
part1 input = length $ filter id $ map nice (lines input)

part2 :: String -> Int
part2 input = length $ filter id $ map nicer (lines input)

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ "part 1: " ++ show (part1 input)
  print $ "part 2: " ++ show (part2 input)


