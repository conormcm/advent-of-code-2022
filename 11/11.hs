import Prelude hiding (round)
import Data.List
import Data.Maybe

data Monkey = Monkey {
  mid :: Int,
  items :: [Int],
  operation :: Int -> Int,
  test :: Int,
  btrue :: Int,
  bfalse :: Int,
  inspected :: Int
}

mapi :: (a -> a) -> Int -> [a] -> [a]
mapi f 0 (x:xs) = f x : xs
mapi f i (x:xs) = x : mapi f (i-1) xs

inspect :: (Int -> Int) -> Monkey -> Int -> [Monkey] -> [Monkey]
inspect reduce monkey item = throw (target monkey) inspected where
  inspected = reduce (operation monkey item)
  target = if inspected `rem` test monkey == 0 then btrue else bfalse
  throw i item = mapi (\m -> m { items = item : items m }) i

turn :: (Int -> Int) -> Int -> [Monkey] -> [Monkey]
turn reduce mid monkeys = finishTurn mid thrown where
  monkey = monkeys !! mid
  thrown = foldl (flip (inspect reduce monkey)) monkeys (items monkey)
  finishTurn =
    mapi (\m -> m { items = [], inspected = inspected m + length (items m) })

round :: (Int -> Int) -> [Monkey] -> [Monkey]
round reduce monkeys = foldl (flip (turn reduce)) monkeys [0..length monkeys-1]

business :: [Monkey] -> Int
business = product . take 2 . reverse . sort . map inspected

main = do
  monkeys <- parseMonkeys . lines <$> readFile "input_full.txt"
  let rounds1 = iterate (round (`quot` 3)) monkeys
  let rounds2 = iterate (round (`rem` foldl lcm 1 (map test monkeys))) monkeys
  print . business $ rounds1 !! 20
  print . business $ rounds2 !! 10000

parseMonkeys :: [String] -> [Monkey]
parseMonkeys [] = []
parseMonkeys lines = parseMonkey monkeyLines : parseMonkeys (drop 1 rest)
  where (monkeyLines, rest) = break (== "") lines

parseMonkey :: [String] -> Monkey
parseMonkey [mid, items, operation, test, btrue, bfalse] =
  Monkey { mid = read . init . last . words $ mid,
           items = parseStarting items,
           operation = parseOperation operation,
           test = read . (!! 3) . words . drop 2 $ test,
           btrue = read . last . words $ btrue,
           bfalse = read . last . words $ bfalse,
           inspected = 0
         }

parseStarting :: String -> [Int]
parseStarting = map (read . init) . drop 2 . words . (++ [',']) . drop 2

parseOperation :: String -> Int -> Int
parseOperation line x = operator (fromMaybe x operand) x
  where [_,_,_,_,op,r] = words (drop 2 line)
        operator = if op == "+" then (+) else (*)
        operand = if r == "old" then Nothing else Just (read r)
