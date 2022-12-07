import Data.Either
import Data.List

data Dir = Dir { name :: String, subdirs :: [Dir], files :: Int }
type Path = [String]

setEntries :: ([Dir], Int) -> Path -> Dir -> Dir
setEntries entries (dirName:path) dir@(Dir name subDirs files)
  | dirName == name && null path = uncurry (Dir name) entries
  | dirName == name = Dir name (map (setEntries entries path) subDirs) files
  | otherwise = dir

process :: Path -> Dir -> [String] -> Dir
process _ dir [] = dir
process path dir ("$ cd ..":lines) = process (init path) dir lines
process path dir (('$':' ':'c':'d':_:name):lines) =
  process (path ++ [name]) dir lines
process path dir (_:lines) = process path (setEntries entries path dir) rest
  where (output, rest) = break ((== '$') . head) lines
        entries = sum <$> partitionEithers (map dirOrFile output)

dirOrFile :: String -> Either Dir Int
dirOrFile ('d':'i':'r':_:name) = Left (Dir name [] 0)
dirOrFile line = Right . read . head . words $ line

dirSizes :: Dir -> (Int, [Int])
dirSizes (Dir _ dirs files) = (sum subSizes + files, subSizes ++ rest)
  where sizes = map dirSizes dirs
        subSizes = map fst sizes
        rest = sizes >>= snd

main :: IO ()
main = do
  root <- process [] (Dir "/" [] 0) . lines <$> readFile "input_full.txt"
  let (total, sizes) = reverse . sort <$> dirSizes root
  let needed = 30000000 - (70000000 - total)
  print . sum . filter (<= 100000) $ sizes
  print . last . filter (>= needed) $ sizes
