import Control.Monad
import Data.Char

main = do
  contents <- getContents
  putStr $ shortLinesOnly contents

-- An interact(ion) can be viewed as:
-- 1. give a program some contents (in the form of string)
-- 2. the program runs a "f :: String -> String" function on it
-- 3. the program returns an output 

-- The "interact" function takes a function of type "f :: String -> String" as parameter and returns an IO action

-- so line 4 to 6 can be re-written as following: 

-- main = interact shortLinesOnly

shortLinesOnly :: String -> String 
shortLinesOnly input = 
  let allLines = lines input
      shortLines = filter (\l -> length l < 10) allLines
      output = unlines shortLines
  in output
