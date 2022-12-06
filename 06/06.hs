import Data.List

subN ::  Int -> [Char] -> [[Char]]
subN n rest = take n rest : subN n (tail rest)

uniquePos :: Int -> [Char] -> Int
uniquePos n line =
  snd . head . dropWhile ((/= n) . length . nub . fst) $ subN n line `zip` [n..]

main :: IO ()
main = do
  input <- readFile "input_full.txt"
  print $ uniquePos 4 input
  print $ uniquePos 14 input
