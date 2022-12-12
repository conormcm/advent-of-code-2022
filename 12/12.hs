import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

type Point = (Int, Int)
type Graph = Map Point (Char, [Point])

charAt :: [[Char]] -> Point -> Char
charAt grid (y, x) = grid !! y !! x

nodesOf :: Char -> Graph -> [Point]
nodesOf c = map fst . filter ((== c) . fst . snd) . Map.toList

adjacent :: Point -> Point -> [Point]
adjacent (y, x) (by, bx) =
  filter (\(y', x') -> 0 <= y' && y' < by && 0 <= x' && x' < bx) .
  map (\(my, mx) -> (y + my, x + mx)) $
  [(1, 0), (0, 1), (-1, 0), (0, -1)]

process :: [[Char]] -> Graph
process grid = foldl (link grid bounds) Map.empty (concat grid `zip` [0..])
  where bounds = (length grid, length (head grid))

link :: [[Char]] -> Point -> Graph -> (Char, Int) -> Graph
link grid bounds nodes (c, i) = Map.insert p (c, neighbors) nodes
  where p = i `quotRem` snd bounds
        neighbors = filter (validMove c . charAt grid) $ adjacent p bounds
        validMove 'S' 'a' = True
        validMove c1  'E' = c1 == 'z'
        validMove c1  c2  = succ c1 == c2 || c1 >= c2

tick :: Graph -> Map Point () -> [Point] -> (Map Point (), [Point])
tick graph seen ps = concat <$> mapAccumL tickPoint seen (map (graph Map.!) ps)
  where tickPoint seen (_, neighbors) =
          let next = filter (`Map.notMember` seen) neighbors
          in (foldr (`Map.insert` ()) seen next, next)

search' :: Int -> Graph -> Point -> Map Point () -> [Point] -> Int
search' _ _     _    _       [] = 0
search' n graph goal seen ps
  | n > 25 && goal `elem` ps = n
  | otherwise = uncurry (search' (n+1) graph goal) (tick graph seen ps)

search :: Graph -> Point -> Point -> Int
search graph goal p = search' 0 graph goal (Map.fromList [(p, ())]) [p]

main :: IO ()
main = do
  graph <- process . lines <$> readFile "input_full.txt"
  let from = search graph (head (nodesOf 'E' graph))
  print $ from (head (nodesOf 'S' graph))
  print . minimum . filter (/= 0) $ map from (nodesOf 'a' graph)
