module Main where

import Data.Array

grid :: Ix t => (t, t) -> Array t Bool
grid bnds = array bnds [(i, False) | i <- range bnds]

limits :: ((Int, Int), (Int, Int))
--limits = ((0,0), (999,999))
limits = ((0,0), (2,2))

toggle :: Ix t => Array t Bool -> t -> Array t Bool
toggle i a = i // [(a, not (i ! a))]

on :: Ix t => Array t Bool -> t -> Array t Bool
on i a = i // [(a, True)]

off :: Ix t => Array t Bool -> t -> Array t Bool
off i a = i // [(a, False)]


-- Usage: mutate (grid limits) [(toggle, (range limits)), (toggle, (range ((1,1),(1,1)) )), (toggle, (range ((2,2),(2,2)) ))]
-- This function applies the listed operations in order on the grid, operating on full grid each time (that is the reason for
-- nested foldl)
mutate :: Ix t => Array t Bool
                    -> [(Array t Bool -> t -> Array t Bool, [t])]
                    -> Array t Bool
mutate = foldl apply
  where
    apply grd (mut, rng) = foldl mut grd rng

lit :: Ix t => Array t Bool -> Int
lit = length . (filter id) . elems

part1 :: String -> Int
part1 input = lit $ mutate (grid limits) [(toggle, (range limits)), (toggle, (range ((1,1),(1,1)) )), (toggle, (range ((2,2),(2,2)) ))]


main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ "part 1: " ++ show (part1 input)
  -- print $ "part 2: " ++ show (part2 input)

