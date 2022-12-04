import Data.List

parse :: String -> [([Int], [Int])]
parse = map parseLine . lines

parseLine :: String -> ([Int], [Int])
parseLine = partitionWith (uncurry enumFromTo . partitionWith read '-') ','

partitionWith :: (String -> a) -> Char -> String -> (a, a)
partitionWith f d text = let (l, _:r) = break (== d) text in (f l, f r)

fullOverlap, partialOverlap :: [Int] -> [Int] -> Bool
fullOverlap r1 r2 = let i = r1 `intersect` r2 in i == r1 || i == r2
partialOverlap r1 r2 = not . null $ r1 `intersect` r2

main :: IO ()
main = do
  input <- parse <$> readFile "input_full.txt"
  print . length . filter (uncurry fullOverlap) $ input
  print . length . filter (uncurry partialOverlap) $ input
