import Data.List

pLen :: Int
pLen = 4

mLen :: Int 
mLen = 14

getMarkerChars :: String -> Int -> Int -> Int 
getMarkerChars s len n 
  | length s < len || nub xs' == xs' = n
  | otherwise                        = getMarkerChars (xs ++ s') len (n + 1)
  where (xs'@(_ : xs), s') = splitAt len s

--------------------------------------------------------------------------------

main :: IO ()
main = do 
  text <- readFile "input6.txt"

  let pChars = getMarkerChars text pLen pLen
  let mChars = getMarkerChars text mLen mLen
 
  putStrLn $ "Start-of-packet marker is at: " ++ show pChars
  putStrLn $ "Start-of-message marker is at: " ++ show mChars