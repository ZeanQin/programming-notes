-- f: String -> String takes a string and output a string. Interact wraps it so that the input String comes from the standard input and then prints the output to standard output 


main = interact shortLinesOnly
-- main = do 
--   contents <- getContents
--   putStr $ shortLinesOnly contents

-- takes a number of lines (delimited by "\n") and returns a string that contains only short lines
shortLinesOnly :: String -> String
shortLinesOnly input = output 
  where allLines = lines input
        shortLines = filter (\l -> length l <= 10) allLines
        output = unlines shortLines
