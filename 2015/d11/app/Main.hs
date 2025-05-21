module Main where

import Data.Char (chr, isAlpha, ord)
import Data.List (group)

increment' :: [Char] -> [Char]
increment' = head . dropWhile (not . valid') . iterate next

valid' :: [Char] -> Bool
valid' s = hasStraight s && noForbidden s && gotPairs s
  where
    hasStraight = (> 0) . length . filter (> 2) . map length . groupBy' (\a b -> (ord a - ord b) == 1)
    noForbidden = all (\c -> not $ c `elem` "iol")
    gotPairs = (> 1) . length . filter (> 1) . map length . group

groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' _ [] = []
groupBy' eq (x0 : xs0) = go [x0] xs0
  where
    go (x : xs) (y : zs) | eq x y = go (y : x : xs) zs
    go g (y : zs) = reverse g : go [y] zs
    go g [] = [reverse g]

next :: [Char] -> [Char]
next (c : cs) = case c of
  'h' -> 'j' : cs
  'n' -> 'p' : cs
  'k' -> 'm' : cs
  'z' -> 'a' : next cs
  _ -> chr (ord c + 1) : cs
next [] = []

part1 :: [Char] -> [Char]
part1 = reverse . increment' . reverse

part2 :: [Char] -> [Char]
part2 = reverse . increment' . next . increment' . reverse

main :: IO ()
main = do
  input <- takeWhile isAlpha <$> readFile "input.txt"
  print $ part1 input
  print $ part2 input
