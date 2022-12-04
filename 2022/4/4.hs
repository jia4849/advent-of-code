import Data.List
import qualified Data.Text as T

textToInt :: [T.Text] -> [Int]
textToInt [x, y] = [read (T.unpack x) .. read (T.unpack y)]

hasSubset :: [[Int]] -> Bool
hasSubset [x, y] = all (`elem` y) x || all (`elem` x) y

hasOverlap :: [[Int]] -> Bool
hasOverlap [x, y] = not $ null $ x `intersect` y 

getAssignments :: T.Text -> [[[Int]]]
getAssignments t = map (map (textToInt . T.splitOn (T.pack "-"))) t'
  where
    t' = map (T.splitOn $ T.pack ",") $ T.lines t

count :: ([[Int]] -> Bool) -> [[[Int]]] -> Int
count = ((length . filter (== True)) .) . map

--------------------------------------------------------------------------------

main :: IO ()
main = do
  text <- fmap T.pack $ readFile "input4.txt"
  let assignments = getAssignments text

  putStrLn $ "Num of subsets: " ++ show (count hasSubset assignments)
  putStrLn $ "Num of overlaps: " ++ show (count hasOverlap assignments)

