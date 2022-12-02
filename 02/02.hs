data Move = Rock | Paper | Scissors deriving (Enum, Eq, Show)
data Result = Loss | Draw | Win deriving (Enum, Show)
data Variable = X | Y | Z deriving (Enum, Read, Show)

beats :: Move -> Move
beats move = [Scissors, Rock, Paper] !! fromEnum move

loses :: Move -> Move
loses move = [Paper, Scissors, Rock] !! fromEnum move

score :: Move -> Move -> Int
score a b | beats a == b = 6 + fromEnum a + 1
          | a == b       = 3 + fromEnum a + 1
          | otherwise    = 0 + fromEnum a + 1

fix :: Move -> Result -> Move
fix move result = [beats move, move, loses move] !! fromEnum result

parseMove :: String -> Move
parseMove "A" = Rock
parseMove "B" = Paper
parseMove "C" = Scissors

parseInput :: String -> [(Move, Variable)]
parseInput = map ((\[move, var] -> (parseMove move, read var)) . words) . lines

convert :: (Enum a, Enum b) => a -> b
convert = toEnum . fromEnum

main :: IO ()
main = do
  input <- parseInput <$> readFile "input_full.txt"
  print . sum . map (\(a, b) -> score (convert b) a) $ input
  print . sum . map (\(a, b) -> score (fix a (convert b)) a) $ input
