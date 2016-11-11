import System.IO  
import System.Directory  
import Data.List 

main = do
  handle <- openFile "./todo.txt" ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents <- hGetContents handle
  let todoTasks = lines contents
      numberedTasks = zipWith (\n task -> show n ++ " - " ++ task) [0..] todoTasks
  putStrLn $ "Current Tasks: "
  putStr $ unlines numberedTasks
  putStrLn $ "Please Enter a number to delete a task: "
  numberString <-getLine 
  let number = read numberString
      newTodoTasks = delete (todoTasks !! number) todoTasks
  hPutStr tempHandle (unlines newTodoTasks)

  hClose handle
  hClose tempHandle

  removeFile "todo.txt" 
  renameFile tempName "todo.txt"
