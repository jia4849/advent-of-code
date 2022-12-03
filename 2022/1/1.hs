import qualified Data.Text as T
import Data.List

getCals :: T.Text -> [Int]
getCals t = map sum cals
  where   
    sep  = T.pack "\n"
    t'   = map (T.splitOn sep) $ T.splitOn (mappend sep sep) t
    cals = map (map (\t -> read (T.unpack t) :: Int)) t'

getTop :: Int -> [Int] -> Int
getTop n = sum . take n . sortBy (flip compare)

--------------------------------------------------------------------------------

main :: IO() 
main = do
  text <- fmap T.pack $ readFile "input.txt"

  let cals = getCals text
  let max  = maximum cals
  let top3 = getTop 3 cals

  putStrLn ("Max calories: " ++ show max)
  putStrLn ("Sum of top 3 calories: " ++ show top3)