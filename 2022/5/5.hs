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

evalInstrs :: [(Int, Int, Int)] -> [String] -> [String]
evalInstrs [] xs     = xs
evalInstrs (i:is) xs = evalInstrs is (evalInstr i xs)
  where 
    evalInstr :: (Int, Int, Int) -> [String] -> [String]
    evalInstr (n, from, to) xs = map snd $ sort $ nubBy (\x y -> fst x == fst y) xs'
      where 
        zipped      = zip [0..] xs
        (x, fromS)  = zipped !! (from - 1)
        (s, fromS') = splitAt n fromS
        (y, toS)    = zipped !! (to - 1)
        xs'         = reverse (zipped ++ [(x, fromS')] ++ [(y, reverse s ++ toS)])

--------------------------------------------------------------------------------
-- Part 2

evalInstrs' :: [(Int, Int, Int)] -> [String] -> [String]
evalInstrs' [] xs     = xs
evalInstrs' (i:is) xs = evalInstrs' is (evalInstr i xs)
  where 
    evalInstr :: (Int, Int, Int) -> [String] -> [String]
    evalInstr (n, from, to) xs = map snd $ sort $ nubBy (\x y -> fst x == fst y) xs'
      where 
        zipped      = zip [0..] xs
        (x, fromS)  = zipped !! (from - 1)
        (s, fromS') = splitAt n fromS
        (y, toS)    = zipped !! (to - 1)
        xs'         = reverse (zipped ++ [(x, fromS')] ++ [(y, s ++ toS)])

--------------------------------------------------------------------------------

main = do 
  t <- readFile "input5.txt"
  let sep = "\n\n"
  let [s, i] = map (map T.unpack . T.lines) $ T.splitOn (T.pack sep) $ T.pack t
  
  let stacks = getStacks s
  let instrs = getInstrs i
  
  let stacks' = evalInstrs instrs stacks
  putStrLn $ "Crates on top: " ++ map head stacks'

  let stacks'' = evalInstrs' instrs stacks
  putStrLn $ "Crates on top (v2): " ++ map head stacks''