import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Text.Read

getStacks :: [String] -> [String]
getStacks ss = map (dropWhile (== ' ')) $ transpose $ init rs
  where 
    rs = map (map snd . filter (\(a, b) -> (a - 1) `mod` 4 == 0) . zip [0..]) ss

getInstrs :: [String] -> [(Int, Int, Int)]
getInstrs t = map ((\[x,y,z] -> (x, y, z)) . mapMaybe readMaybe . words) t

evalInstrs :: [String] -> [(Int, Int, Int)] -> [String]
evalInstrs = foldl' evalInstr
  where 
    evalInstr :: [String] -> (Int, Int, Int) -> [String]
    evalInstr xs (n, from, to) = map snd $ sort $ nubBy (\x y -> fst x == fst y) xs'
      where 
        zipped      = zip [0..] xs
        (x, fromS)  = zipped !! (from - 1)
        (s, fromS') = splitAt n fromS
        (y, toS)    = zipped !! (to - 1)
        xs'         = (x, fromS') : (y, reverse s ++ toS) : zipped

--------------------------------------------------------------------------------
-- Part 2

evalInstrs' :: [String] -> [(Int, Int, Int)] -> [String]
evalInstrs' = foldl' evalInstr
  where 
    evalInstr :: [String] -> (Int, Int, Int) ->  [String]
    evalInstr xs (n, from, to) = map snd $ sort $ nubBy (\x y -> fst x == fst y) xs'
      where 
        zipped      = zip [0..] xs
        (x, fromS)  = zipped !! (from - 1)
        (s, fromS') = splitAt n fromS
        (y, toS)    = zipped !! (to - 1)
        xs'         = (x, fromS') : (y, s ++ toS) : zipped

--------------------------------------------------------------------------------

main :: IO ()
main = do 
  t <- readFile "input5.txt"
  let sep = "\n\n"
  let [s, i] = map (map T.unpack . T.lines) $ T.splitOn (T.pack sep) $ T.pack t
  
  let stacks = getStacks s
  let instrs = getInstrs i
  
  let stacks' = evalInstrs stacks instrs 
  putStrLn $ "Crates on top: " ++ map head stacks'

  let stacks'' = evalInstrs' stacks instrs 
  putStrLn $ "Crates on top (v2): " ++ map head stacks''