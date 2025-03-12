module Main where

import D03
import Test.HUnit
import qualified System.Exit as Exit

tests :: Test
tests = TestList 
  [ TestLabel "test1" $ TestCase $ assertEqual "to 2 houses"  2 $ part1 ">"
  , TestLabel "test2" $ TestCase $ assertEqual "to square"    4 $ part1 "^>v<"
  , TestLabel "test3" $ TestCase $ assertEqual "to lucky two" 2 $ part1 "^v^v^v^v^v"
  , TestCase $ assertEqual "p1" 3  $ part2 "^v"
  , TestCase $ assertEqual "p2" 11 $ part2 "^v^v^v^v^v"
  ]


main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess


