import System.IO
import Data.Char

main = do
  contents <- readFile "./zean.txt"
  writeFile "./zeanCapslock.txt" (map toUpper contents)
