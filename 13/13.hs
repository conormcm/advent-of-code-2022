import Data.List

data Element = EInt Int | HList [Element] deriving Eq

instance Ord Element where
  compare (EInt x) (EInt y) = x `compare` y
  compare (HList xs) (HList ys) = case find (/= EQ) (zipWith compare xs ys) of
    Just ordering -> ordering
    Nothing -> length xs `compare` length ys
  compare (EInt x) (HList ys) = HList [EInt x] `compare` HList ys
  compare (HList xs) (EInt y) = HList xs `compare` HList [EInt y]

push :: Int -> Element -> [Element] -> [Element]
push 0 x xs         = xs ++ [x]
push d x [HList xs] = [HList (push (d-1) x xs)]
push d x (y:xs)     = y : push d x xs

parseHList :: String -> Element
parseHList = head . fst . foldl parseEl ([], 0) . (zip <*> tail) where
  parseEl (xs, d) ('[', _)   = (push d (HList []) xs, d+1)
  parseEl (xs, d) (']', _)   = (xs, d-1)
  parseEl (xs, d) (',', _)   = (xs, d)
  parseEl (xs, d) ('1', '0') = (push d (EInt 10) xs, d)
  parseEl (xs, d) (c, _)     = (push d (EInt (read [c])) xs, d)

parse :: [String] -> [(Element, Element)]
parse []           = []
parse (l1:l2:rest) = (parseHList l1, parseHList l2) : parse (drop 1 rest)

main :: IO ()
main = do
  pairs <- parse . lines <$> readFile "input_full.txt"
  let dividers = [HList [HList [EInt 2]], HList [HList [EInt 6]]]
  let sorted = sort (uncurry (++) (unzip pairs) ++ dividers)
  print . sum . map (+1) . elemIndices LT . map (uncurry compare) $ pairs
  print . product . map (+1) . findIndices (`elem` dividers) $ sorted
