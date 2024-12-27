main = do
  s <- readFile "input.txt"
  let out = foldr (+) 0 (map parse s)
  print out


parse :: Char -> Int
parse '(' = 1
parse ')' = -1
parse '\n' = 0
