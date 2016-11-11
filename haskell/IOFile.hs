import System.IO

main = do
  handle <- openFile "./zean.txt" ReadMode
  contents <- hGetContents handle
  putStr contents
  hClose handle
