import Data.List
import Data.Maybe

type Cmd = (String, Int)
type Pos = (Int, Int)

initial :: [[(Int, Int)]]
initial = repeat [(0, 0)]

getCmds :: String -> [Cmd]
getCmds = map ((\[a,b] -> (a, read b)) . words) . lines

getDist :: Pos -> Pos -> Float
getDist (x, y) (x', y') = sqrt $ fromIntegral ((x - x') ^ 2 + (y - y') ^ 2) 

getDiags :: Pos -> [Pos]
getDiags (x, y) = [(x + m, y + n) | m <- [1, -1], n <- [1, -1]]

getDirOf :: Int -> Pos -> Pos -> Maybe (String)
getDirOf n h p@(x, y) = lookup h (zip direct dirs)
  where 
    direct = [(x - n, y), (x + n, y), (x, y + n), (x, y - n)] ++ getDiags p
    dirs   = ["L", "R", "U", "D", "RU", "RD", "LU", "LD"]

updatePos :: String -> Pos -> Pos
updatePos "" p          = p
updatePos (s:ss) (x, y) = case s of
  'L' -> updatePos ss (x - 1, y) 
  'R' -> updatePos ss (x + 1, y)
  'U' -> updatePos ss (x, y + 1)
  'D' -> updatePos ss (x, y - 1)

-- For each adjacent pair, updates the second item with respect to the first 
updateState :: [[Pos]] -> [[Pos]]
updateState [x]                         = [x]
updateState (xs@(x:_) : ys@(y:_) : xys) = xs : updateState ((y' : ys) : xys)
  where 
    d = getDirOf 2 x y
    y'
      | getDist x y < 2 = y
      | isJust d        = updatePos (fromJust d) y
      | otherwise       = head $ filter (isJust . getDirOf 1 x) (getDiags y)

-- For each step of the command, updates the head and then calls update state
processCmds :: [[Pos]] -> [Cmd] -> [[Pos]]
processCmds = foldl' processCmd
  where 
    processCmd :: [[Pos]] -> Cmd -> [[Pos]]
    processCmd s (_, 0) = s
    processCmd (hs@(h:_) : s) (d, n) = processCmd s' (d, n - 1)
      where 
        hs' = (updatePos d h) : hs
        s'  = updateState (hs' : s)
        
--------------------------------------------------------------------------------

main = do 
  text <- readFile "input9.txt"
  
  let cmds = getCmds text

  -- Part 1
  let state' = processCmds (take 2 initial) cmds
  let t = last state'
  putStrLn $ "Positions visited at least once: " ++ show (length $ nub t)

  -- Part 2
  let state'' = processCmds (take 10 initial) cmds
  let t' = last state''
  putStrLn $ "Positions visited at least once (v2): " ++ show (length $ nub t')