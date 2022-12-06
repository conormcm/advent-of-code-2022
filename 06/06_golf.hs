import Data.List
solve i n xs = if (==) <*> nub $ take n xs then i else solve (i+1) n (tail xs)
main = readFile "input_full.txt" >>=
  (\input -> print (solve 4 4 input) >> print (solve 14 14 input))
