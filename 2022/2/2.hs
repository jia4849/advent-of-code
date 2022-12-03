import Data.Maybe 

win :: Int
win = 6

draw :: Int
draw = 3

loss :: Int 
loss = 0

letterToChoice = [
  ("A", "Rock"),
  ("B", "Paper"),
  ("C", "Scissors"),
  ("X", "Rock"),
  ("Y", "Paper"),
  ("Z", "Scissors")
  ]

choiceToInt = [
  ("Rock", 1),
  ("Paper", 2),
  ("Scissors", 3)
  ]

--------------------------------------------------------------------------------
-- Part 2

toWin = [
  ("Rock", "Paper"),
  ("Paper", "Scissors"),
  ("Scissors", "Rock")
  ]

toLose = [
  ("Paper", "Rock"),
  ("Scissors", "Paper"),
  ("Rock", "Scissors")
  ] 

--------------------------------------------------------------------------------

lookUp :: Eq a => a -> [(a, b)] -> b
lookUp = (fromJust .) . lookup

getRounds :: String -> [[String]]
getRounds = map (map (`lookUp` letterToChoice) . words) . lines

getRounds' :: String -> [[String]]
getRounds' = map ((\[x,y] -> [lookUp x letterToChoice, y]) . words) . lines

evalRound :: [String] -> Int
evalRound [x, y]
  | x == y = score + draw
  | x == "Rock" && y == "Paper" 
    || x == "Paper" && y == "Scissors" 
    || x == "Scissors" && y == "Rock" = score + win
  | otherwise = score + loss
  where score = lookUp y choiceToInt

fixRound :: [String] -> Int
fixRound [x, y]
  | y == "X" = lookUp (lookUp x toLose) choiceToInt + loss
  | y == "Y" = lookUp x choiceToInt + draw
  | otherwise = lookUp (lookUp x toWin) choiceToInt + win

--------------------------------------------------------------------------------

main :: IO ()
main = do
  text <- readFile "input2.txt"

  let score = sum $ map evalRound $ getRounds text
  let score' = sum $ map fixRound $ getRounds' text

  putStrLn $ "Total score: " ++ (show score)
  putStrLn $ "Total score from fixing the rounds: " ++ (show score')