import Data.Char
import Data.List

split :: [a] -> ([a], [a])
split line = let n = length line `quot` 2 in (take n line, drop n line)

chunk :: [a] -> [[a]]
chunk (x : y : z : rest) = [x, y, z] : chunk rest
chunk [] = []

score :: Char -> Int
score c | 'a' <= c && c <= 'z' = (ord c - ord 'a') + 1
        | 'A' <= c && c <= 'Z' = (ord c - ord 'A') + 27

main :: IO ()
main = do
  input <- lines <$> readFile "input_full.txt"
  print . sum . map (score . head . uncurry intersect . split) $ input
  print . sum . map (score . head . foldl1 intersect) . chunk $ input
