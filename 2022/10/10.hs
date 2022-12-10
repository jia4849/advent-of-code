import Data.List
import qualified Data.Text as T

type Instr = Int -> Int

parseInstrs :: [String] -> [Instr]
parseInstrs = concatMap parseInstr
  where
    parseInstr :: String -> [Instr]
    parseInstr s
      | "noop" `isPrefixOf` s = [id]
      | "addx" `isPrefixOf` s = [id, (+) (read $ last $ words s)]

evalInstrs :: Int -> [Instr] -> [Int]
evalInstrs i = map (\f -> f i) . (scanl' (.) id)

getSprite :: Int -> Int -> Int -> [Int]
getSprite w h mid = filter (`elem` row) [mid - 1, mid, mid + 1]
  where 
    quot = mid `div` w
    row  = [quot * w .. (quot + 1) * w - 1]

getScreen :: (Int, Int) -> [Int] -> String
getScreen (w, h) states = T.unpack $ T.unlines (T.chunksOf w $ T.pack screen)
  where   
    drawing = concat (replicate h [0.. w - 1])
    sprites = map (getSprite w h) states
    screen = zipWith (\d s -> if d `elem` s then '#' else '.') drawing sprites

--------------------------------------------------------------------------------

main :: IO ()
main = do 
  instrs <- parseInstrs . lines <$> readFile "input10.txt"
  let states = evalInstrs 1 instrs 
  
  -- Part 1
  let cycles = [20, 60, 100, 140, 180, 220]
  let sigStrengths = zipWith (*) (map (\c -> states !! (c - 1)) cycles) cycles
  putStrLn $ "Sum of signal strengths: " ++ (show $ sum sigStrengths)

  -- Part 2
  let size = (40, 6)
  let screen = getScreen size states
  putStrLn $ "\nScreen:\n" ++ screen

