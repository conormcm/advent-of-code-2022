import Control.Arrow
import Data.List
import Data.Maybe

type Stack = [Char]
type Move = (Int, Int, Int)

moveCrates  :: Move -> [Stack] -> [Stack]
moveCrates (n, src, dst) stacks =
  let (init1, srcCrate:tail1) = splitAt (src-1) stacks
      stacks' = init1 ++ (drop n srcCrate) : tail1
      (init2, dstCrate:tail2) = splitAt (dst-1) stacks'
  in  init2 ++ (take n srcCrate ++ dstCrate) : tail2

oneAtATime  :: Move -> [Stack] -> [Stack]
oneAtATime (n, src, dst) = head . drop n . iterate (moveCrates (1, src, dst))

parse :: [String] -> ([Stack], [Move])
parse = (stacks *** moves) . break (== "")
  where stacks = map catMaybes . transpose . map parseStackLine . init
        moves = map parseMove . tail

parseStackLine :: String -> [Maybe Char]
parseStackLine []                 = []
parseStackLine (' ':' ':' ':rest) = Nothing : parseStackLine (drop 1 rest)
parseStackLine ('[':s:']':rest)   = Just s : parseStackLine (drop 1 rest)

parseMove :: String -> Move
parseMove line = case words line of
  [_, n, _, src, _, dst] -> (read n, read src, read dst)

main :: IO ()
main = do
  (stacks, moves) <- parse . lines <$> readFile "input_full.txt"
  putStrLn . map head $ foldl (flip oneAtATime) stacks moves
  putStrLn . map head $ foldl (flip moveCrates) stacks moves
