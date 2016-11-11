import Control.Monad
import Data.Char

main = interact respondPalindromes

respondPalindromes :: String -> String 
respondPalindromes = unlines.map (\a -> if (isPalindromes a) then "palindrome" else "not palindrome").lines  
  where isPalindromes x = x == reverse x
