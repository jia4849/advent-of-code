import Data.List 
import Data.Maybe

ps :: [(Char, Int)]
ps = zip (['a'..'z'] ++ ['A'..'Z']) [1..52]

lookUp :: Eq a => a -> [(a, b)] -> b 
lookUp = (fromJust . ) . lookup

getCompartments :: [String] -> [(String, String)]
getCompartments = map (\s -> splitAt ((length s + 1) `div` 2) s)

getPriorities :: [(String, String)] -> [Int]
getPriorities = map (\(x, y) -> sum $ nub $ map (`lookUp` ps) (intersect x y))

--------------------------------------------------------------------------------
 
getGroupsOf :: Int -> [String] -> [[String]]
getGroupsOf n xs 
  | length xs <= n = [xs]
  | otherwise      = a : (getGroupsOf n b)
  where (a, b) = splitAt n xs

getPriorities' :: [[String]] -> [Int]
getPriorities' sss = map (sum . nub . map (`lookUp` ps) . foldr1 intersect) sss

--------------------------------------------------------------------------------

main :: IO () 
main = do 
  rs <- fmap lines $ readFile "input3.txt"

  let cs = getCompartments rs
  putStrLn $ "Sum of the priorities: " ++ show (sum (getPriorities cs))

  let gs = getGroupsOf 3 rs
  putStrLn $ "Sum of the grouped priorities: " ++ show (sum (getPriorities' gs))
