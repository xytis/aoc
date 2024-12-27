main = do
  s <- readFile "input.txt"
  -- let s = ")"
  -- let s = "()())"
  let p = map parse s
  let out = foldl xsum (0, 0) (zip [1..] p)
  print out

parse :: Char -> Int
parse '(' = 1
parse ')' = -1
parse '\n' = 0

xsum (acc, first) (i, el) 
  | acc >= 0 = (acc + el, i)
  | acc < 0 = (acc, first)
