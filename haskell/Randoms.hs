import System.Random
import Data.List

-- generating a single random value
a = random (mkStdGen 100) :: (Float, StdGen)
b = random (mkStdGen 100) :: (Bool, StdGen)
c = random (mkStdGen 100) :: (Integer, StdGen)

-- generating a single random value within a range
aa = randomR (1, 6) (mkStdGen 100) :: (Float, StdGen)
bb = randomR (1, 6) (mkStdGen 100) :: (Int, StdGen)

-- generating a specified number of random values
d = take 5 $ randoms (mkStdGen 100) :: [Float]
e = take 5 $ randoms (mkStdGen 100) :: [Bool]
f = take 5 $ randoms (mkStdGen 100) :: [Integer]

-- generating a finite list of random values
dd = take 5 $ randomRs ('a', 'z') (mkStdGen 100) :: [Char]

main = do
  gen <- getStdGen
  let randomChars = randomRs ('a', 'z') gen
      (first, rest) = splitAt 20 randomChars
      (second, _) = splitAt 20 rest
  putStrLn first
  putStrLn second
