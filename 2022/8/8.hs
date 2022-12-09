import Data.Char
import Data.List 
import qualified Data.Text as T

--------------------------------------------------------------------------------
-- Part 1 

-- For list of ints, returns whether the last element is visible
isVisible :: [Int] -> Bool
isVisible xs = all (< x) xs'
  where (x:xs') = reverse xs

-- For list of int pairs, return all visible elements
getVisible :: [(Int, Int)] -> [(Int, Int)]
getVisible = map last . filter (isVisible . map snd) . tail . inits 

-- Get list of list of int pairs
getIntPairs :: [String] -> [[(Int, Int)]]
getIntPairs strs = [zip [n * len + 1 ..]ints | (n, ints) <- zip [1..] intss]
  where
    len = length intss 
    intss = map (map digitToInt) strs -- :: [[Int]]
    pairs = zip [1..] intss

getPerms :: [[a]] -> [[[a]]]
getPerms r = [l, r, u, d]
  where 
    l = map reverse r
    d = transpose r
    u = map reverse d

undoPerms :: [[[a]]] -> [[[a]]]
undoPerms [l, u, d] = [map reverse l, transpose $ map reverse u, transpose d]

--------------------------------------------------------------------------------
-- Part 2

getInts :: [String] -> [[Int]]
getInts = map (map digitToInt)

-- Returns viewing dist for each tree in a row of trees
getViewDists :: [Int] -> [Int]
getViewDists = map (`getIntViewDist` 0) . filter (not . null) . tails
  where 
    getIntViewDist :: [Int] -> Int -> Int
    getIntViewDist [x] n = n
    getIntViewDist (x:y:ys) n
      | y < x     = getIntViewDist (x:ys) (n + 1)
      | otherwise = n + 1

--------------------------------------------------------------------------------
main :: IO () 
main = do
  text <- fmap lines $ readFile "input8.txt"

  -- Part 1
  let is = getIntPairs text
  let ps = getPerms is
  let vs = concatMap (concatMap getVisible) ps
  let vs' = nubBy (\(x, _) (y, _) -> x == y) vs
  putStrLn $ "Trees visible from outside the grid: " ++ show (length vs')

  -- Part 2
  let is' = getInts text
  let [l, r, u, d] = map (map getViewDists) $ getPerms is'
  let [l', u', d'] = undoPerms [l, u, d]
  let vds = zipWith4 (zipWith4 (\a b c d -> product [a, b, c, d])) l' r u' d'
  putStrLn $ "Max viewing distance : " ++ show (maximum $ concat vds)