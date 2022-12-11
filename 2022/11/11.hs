import Data.List
import Data.Maybe 
import qualified Data.Text as T
 
-- Part 1

type Info = (Int, Integer -> Integer, Integer -> Bool, Int, Int)

strToOp = 
  [ ("+", (+)),
    ("-", (-)),
    ("*", (*))
    ]

lookUp :: Eq a => a -> [(a, b)] -> b
lookUp = (fromJust . ) . lookup

splitOn :: String -> String -> [String]
splitOn x y = map T.unpack $ T.splitOn (T.pack x) (T.pack y)

stripPre :: Eq a => [a] -> [a] -> [a]
stripPre = (fromJust . ) . stripPrefix

format :: String -> [[String]]
format = map (map (T.unpack . T.strip) . T.lines) . T.splitOn sep . T.pack 
  where sep = T.pack "\n\n"

getMonkeys :: [String] -> ([Int], (Integer -> Integer, Integer -> Bool, Int, Int))
getMonkeys [_,a,b,c,d,e] = (items, (op, test, throwT, throwF))
  where
    items            = map read $ splitOn "," $ stripPre "Starting items: " a
    [f, n]           = words $ stripPre "Operation: new = old " b
    op               = if n == "old" then (^ 2) else ((lookUp f strToOp) (read n)) 
    test             = (==) 0 . (`mod` (read $ stripPre "Test: divisible by " c))
    [throwT, throwF] = map (read . last . words) [d, e]

inspect :: [[([Int], Integer)]] -> Info -> [[([Int], Integer)]]
inspect lst info@(n, op, test, throwT, throwF)
  | null $ lst !! n  = lst
  | otherwise        = inspect lst'' info
  where 
    (i@(ns, w) : is) = lst !! n
    i'@(_, w')       = (n:ns, (op w) `div` 3)
    throw 
      | test w'   = throwT
      | otherwise = throwF
    (ls, l : ls')    = splitAt throw lst
    lst'             = ls ++ ((i' : l) : ls')
    (r, nth : r')    = splitAt n lst'
    lst''            = r ++ (delete i nth) : r'
 
playRounds :: [[([Int], Integer)]] -> [Info] -> Int -> [[([Int], Integer)]]
playRounds lst _ 0     = lst
playRounds lst infos n = playRounds (foldl inspect lst infos) infos (n - 1)

count :: [[([Int], Integer)]] -> [Int]
count = map length . group . sort . concatMap fst . concat

getMonkeyBusiness :: [[([Int], Integer)]] -> Int
getMonkeyBusiness = product . take 2 . sortBy (flip compare) . count

--------------------------------------------------------------------------------
-- Part 2

modProduct :: Integer
modProduct = 11 * 19 * 5 * 2 * 13 * 7 * 3 * 17

inspect' :: [[([Int], Integer)]] -> Info -> [[([Int], Integer)]]
inspect' lst info@(n, op, test, throwT, throwF)
  | null $ lst !! n  = lst
  | otherwise        = inspect' lst'' info
  where 
    (i@(ns, w) : is) = lst !! n
    i'@(_, w')       = (n:ns, (op w) `mod` modProduct)
    throw 
      | test w'   = throwT
      | otherwise = throwF
    (ls, l : ls')    = splitAt throw lst
    lst'             = ls ++ ((i' : l) : ls')
    (r, nth : r')    = splitAt n lst'
    lst''            = r ++ (delete i nth) : r'
 
playRounds' :: [[([Int], Integer)]] -> [Info] -> Int -> [[([Int], Integer)]]
playRounds' lst _ 0     = lst
playRounds' lst infos n = playRounds' (foldl inspect' lst infos) infos (n - 1)
  
--------------------------------------------------------------------------------

main :: IO ()
main = do
  text <- format <$> readFile "input11.txt"

  let (mItems, mInfos) = unzip $ map getMonkeys text
  let items = map (map ((\x -> ([], x)) . toInteger)) mItems
  let infos = zipWith (\n (a,b,c,d) -> (n,a,b,c,d)) [0..] mInfos

  -- Part 1
  let items' = playRounds items infos 20
  let mb' = getMonkeyBusiness items'
  print $ count items'
  putStrLn $ "Monkey business: " ++ show mb'

  -- Part 2
  let items'' = playRounds' items infos 10000
  let mb'' = getMonkeyBusiness items''
  print $ count items''
  putStrLn $ "Monkey business (v2): " ++ show mb''