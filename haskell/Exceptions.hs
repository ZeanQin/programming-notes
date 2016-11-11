import System.Environment
import System.IO
import System.IO.Error

main = do
  catchIOError countLines handler

countLines :: IO ()
countLines = do 
  (fileName:_) <- getArgs
  contents <- readFile fileName
  putStrLn $ show $ length $ lines contents

handler :: IOError -> IO ()
handler e 
  | isDoesNotExistError e = putStrLn "The file doesn't exist!" 
  | isPermissionError e = putStrLn "You dont have enough permissions to access the file"
  | otherwise = ioError e
