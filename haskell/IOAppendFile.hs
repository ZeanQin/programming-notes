import System.IO

main = do
  l <- getLine
  appendFile "./todo.txt" (l++"\n")
