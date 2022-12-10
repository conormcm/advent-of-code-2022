run :: Int -> [String] -> [Int]
run _ []                = []
run x ("noop" : inputs) = x : run x inputs
run x (addx : inputs)   = x : x : run (x+y) inputs
  where y = read . last . words $ addx

pixel :: Int -> Int -> Char
pixel i value = if abs ((i `rem` 40) - value) <= 1 then '#' else '.'

nl :: String -> String
nl []  = []
nl str = take 40 str ++ ('\n' : nl (drop 40 str))

main :: IO ()
main = do
  values <- run 1 . lines <$> readFile "input_full.txt"
  let strengths = zipWith (*) [1..] values
  print . sum . map ((strengths !!) . pred) $ [20, 60, 100, 140, 180, 220]
  putStr . nl . zipWith pixel [0..] $ values
