fib :: Int -> [Integer]
fib 0 = []
fib 1 = [0]
fib n
  | n > 1 = 0:1:fib 0 1 (n-2)

fib1 first second 0 = []
fib1 first second count
  = (first + second):fib1 second (first+second) (count-1)

allfibs :: [Integer]
allfibs = 0:1:(zipWith (+) allfibs (tail allfibs))


