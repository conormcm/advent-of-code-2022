import Data.List

split :: Eq a => a -> [a] -> [[a]]
split d xs = group [] xs where
  group []    [] = []
  group accum [] = [accum]
  group accum (x : rest)
    | x == d    = accum : group [] rest
    | otherwise = group (accum ++ [x]) rest

parseInput :: String -> [[Int]]
parseInput = map (map read) . split "" . lines

highest :: [[Int]] -> Int
highest = maximum . map sum

highestN :: Int -> [[Int]] -> [Int]
highestN n = take n . sortBy (flip compare) . map sum

main :: IO ()
main = do
  input <- parseInput <$> readFile "input_full.txt"
  print $ highest input
  print $ sum (highestN 3 input)
