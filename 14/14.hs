import Data.List
import Data.Maybe
import qualified Data.Set as Set

type Point = (Int, Int)
type Grid = Set.Set Point

parse :: String -> [[Point]]
parse = map ((>>= parse') . words) . lines where
  parse' coord = if coord == "->" then [] else [read ("(" ++ coord ++ ")")]

drawLine :: Grid -> Point -> Point -> Grid
drawLine grid (fx, fy) (tx, ty)
  | fx == tx = draw $ repeat fx `zip` [fy,next fy..ty]
  | fy == ty = draw $ [fx,next fx..tx] `zip` repeat fy
  where draw = foldl (flip Set.insert) grid
        next = if fx < tx || fy < ty then succ else pred

drawPath :: Grid -> [Point] -> Grid
drawPath grid = foldl (uncurry . drawLine) grid . (zip <*> tail)

dropSand :: (Point -> Bool) -> Grid -> Int -> Point -> Either Grid Int
dropSand end grid bottom (px, py)
  | end (px', py') = Right $ Set.size grid
  | (px, py) == (px', py') = Left $ Set.insert (px', py') grid
  | otherwise = dropSand end grid bottom (px', py')
  where moves = [(px, py+1), (px-1, py+1), (px+1, py+1)]
        (px', py') = fromMaybe (px, py) (find space moves)
        space (px, py) = (py /= bottom + 2) && (px, py) `Set.notMember` grid

fill :: (Point -> Bool) -> Grid -> Int -> Int
fill end grid bottom = case dropSand end grid bottom (500, 0) of
  Left grid' -> fill end grid' bottom
  Right size -> size

main :: IO ()
main = do
  grid <- foldl drawPath Set.empty . parse <$> readFile "input_full.txt"
  let bottom = maximum . Set.map snd $ grid
  print $ fill ((> bottom) . snd) grid bottom - Set.size grid
  print $ fill (== (500, 0)) grid bottom - Set.size grid + 1
