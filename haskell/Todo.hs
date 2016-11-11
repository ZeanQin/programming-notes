import System.Directory
import System.IO
import Data.List
import System.Environment

dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("add", add)
           , ("view", view)
           , ("remove", remove)
           , ("bump", bump)]

main = do
  (command:args) <- getArgs
  let (Just action) = lookup command dispatch
  action args

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")

view :: [String] -> IO ()
view [fileName] = do
  contents <- readFile fileName
  let allLines = lines contents
      numberedTasks = zipWith (\n l -> show n ++ " - " ++ l) [0..] allLines
  putStr $ unlines numberedTasks

remove :: [String] -> IO ()
remove [fileName, numberString] = do
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents <- readFile fileName
  let allLines = lines contents
      number = read numberString
      newTodoItems = delete (allLines !! number) allLines
  hPutStr tempHandle $ unlines newTodoItems
  hClose tempHandle
  removeFile fileName
  renameFile tempName fileName

bump :: [String] -> IO () 
bump [fileName, numberString] = do
  contents <- readFile fileName
  let allLines = lines contents
      number = read numberString
      bumpedList = (allLines !! number):(delete (allLines !! number) allLines)
  (tempName, tempHandle) <- openTempFile "." "temp"
  hPutStr tempHandle $ unlines bumpedList
  hClose tempHandle
  removeFile fileName
  renameFile tempName fileName
