import Data.List
import Data.Maybe
import qualified Data.Text as T

data DirTree = File String Int | Dir String [DirTree]

data Command = CD String | LS [DirTree]

emptyDT :: DirTree
emptyDT = Dir "/" []

totalDiskSpace :: Int
totalDiskSpace = 70000000

unusedSpace :: Int
unusedSpace = 30000000

--------------------------------------------------------------------------------
-- Functions for handling commands and directory trees

-- Parses text into list of commands
parseCmds :: T.Text -> [Command]
parseCmds = tail . map (parseCmd . T.unpack) . tail . T.splitOn (T.pack "$ ")
  where 
    parseCmd :: String -> Command
    parseCmd s 
      | "cd" `isPrefixOf` s = CD (last $ words s)
      | otherwise           = LS c'
      where 
        c  = map words $ tail $ lines s
        c' = map (\[x, y] -> if x == "dir" then Dir y [] else File y (read x)) c

-- Checks if a directory tree is a directory
isDir :: DirTree -> Bool
isDir (Dir _ _) = True
isDir _         = False

-- Inserts list of dts into dt at specified directory in dt
-- Pre: input string is the path to the directory
insertAt :: [String] -> [DirTree] -> DirTree -> DirTree
insertAt _ _ f@(File _ _) 
  = f
insertAt [s] dts' (Dir dn dts) 
  = Dir dn (dts ++ dts')
insertAt (_:sss@(s:_)) dts' (Dir dn dts) 
  = Dir dn ((insertAt sss dts' $ head nextD) : remainingFiles)
  where 
    (ds, fs)         = partition isDir dts
    (nextD, otherDs) = partition (\(Dir s' _) -> s' == s) ds
    remainingFiles   = fs ++ otherDs 

-- Processes commands "cd" or "ls"
-- Returns updated tree
processCmds :: [Command] -> DirTree -> DirTree
processCmds cs dt = processCmds' cs ["/"] dt 
  where 
    processCmds' :: [Command] -> [String] -> DirTree -> DirTree
    processCmds' [] _ dt                  = dt
    processCmds' (c:cs) ddns@(_ : dns) dt = case c of 
      CD ".." -> processCmds' cs dns dt
      CD dn'  -> processCmds' cs (dn' : ddns) dt
      LS ds   -> processCmds' cs ddns (insertAt (reverse ddns) ds dt)     
    
-- Gets sizes of all directories in directory tree
getDirSizes :: DirTree -> [(String, Int)]
getDirSizes (File _ _) = []
getDirSizes dt@(Dir dn dts) = (dn, getDirSize dt) : (concatMap getDirSizes dts)
  where 
    getDirSize :: DirTree -> Int
    getDirSize (File _ n)  = n
    getDirSize (Dir _ dts) = sum $ map getDirSize dts

--------------------------------------------------------------------------------

main :: IO () 
main = do
  text <- readFile "input7.txt"

  let cmds = parseCmds $ T.pack text
  let dt = processCmds cmds emptyDT
  let dirSizes = getDirSizes dt
  let sizes = map snd dirSizes

  -- Part 1
  let sumFiltered = sum $ filter (<= 100000) sizes
  putStrLn $ "Sum of total sizes (each < 100000) of dirs: " ++ show sumFiltered

  -- Part 2
  let total = head sizes
  let maxSpace = totalDiskSpace - unusedSpace
  let minFiltered = minimum $ filter (\n -> (total - n) <= maxSpace) sizes 
  putStrLn $ "Size of smallest deletable directory: " ++ show minFiltered