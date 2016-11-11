-- import Data.List 
-- import Data.List (nub, sort) -- only import the nub and sort functions
-- import Data.List hiding (nub) -- import everything except the nub function
-- import qualified Data.List as M -- functions can be used as M.nub
import qualified Data.Map as Map
import Data.Char
import Control.Monad

-- PARTIALLY APPLIED FUNCTIONS
multiThree :: (Num a) => a -> a -> a -> a
multiThree x y z = x*y*z 

multiTwo = multiThree 9 -- multiTwo is a partially applied function that takes two parameters

resulta = multiTwo 2 3
resultb = multiTwo 3 6

-- a function that takes another function as parameter
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

a = applyTwice (+3) 10 --16
b = applyTwice (++ " world") "hello" --"hello world world" 
c = applyTwice (3:) [1] -- [3, 3, 1]

-- MAP, FILTER, TAKEWHILE and LIST COMPREHENSIONS
-- Intuition: 
---- the map function maps elements of a list to another list, 
---- the filter function filters the elements of a list and doesn't change the value of those elements, 
---- the takeWhile function returns the first part of a list where all elements satisfies a predicate
---- list comprehension is a syntactic sugar list manipulation
-- map takes a function and a list and performs the function on each element of a list
-- map :: (a -> b) -> [a] -> [b]
e = map (+2) [1, 2, 3] -- [3, 4, 5]

-- filter takes a predicate (a function that tells whether something is true) and a list as arguments and returns the list of elements that satisfy the predicate 
f = filter (even) [1, 2, 3, 6] -- [2, 6]

-- takeWhile takes a predicate and goes from the beginning of the list and returns its elements while the predicate holds true
g = takeWhile (<2) [1, 2, 3] -- [1]

-- list comprehension is a syntactic sugar for constructing a new list 
h = [ 2*x | x <- [1..10], mod x 2 == 0]

-- FOLD and SCAN
--
-- Intuition of fold (foldl, foldr, foldl1, foldr1) and scan (scanl, scanr, scanl1, scanr1)
--
-- Fold can be used to implement any function(map, filter, product etc) where you traverse through a list once, element by element, and then return something based on that. foldl and foldr takes 3 arguments: a fold function, an accumulator and a list. foldl consumes the list from the left while foldr consumes the list from the right side. foldl1 and foldr1 are the same as foldl and foldr only that they assumes the list has at least one element. 
--
-- Scan is the same as fold (scanl, scanr, scanl1, scanr1) only that it reports the intermediate value of the accumulator so that you can track the execution of a list function. 
--
-- Below is an example of implementing the product function using fold. 

productl :: (Num a) => [a] -> a
productl = foldl (\acc e -> acc*e) 1 -- NOTE: in the lamda function, acc(umulator) comes before e(lement)

productr :: (Num a) => [a] -> a
productr = foldr (\e acc -> e*acc) 1 -- NOTE: in the lamda function, e(lement) comes before acc(umulator) 

-- LAMBDAS
-- Intuition: lambdas are inline functions that will only be used once
i = map (\(a, b) -> a + b) [(1, 2), (3, 3)]

-- FUNCTION APPLICATION ($)
-- A convinient function so we don't have many parentheses. It can be thought of as writing an opening parenthese and then writing a closing one on the far right side of the expression. 

j = sqrt (2 + 3 + 4) 
k = sqrt $ 2 + 3 + 4 -- sqrt 2 + 3 + 4 will be (sqrt 2) + 3 + 4

-- FUNCTION COMPOSITION
l = sum (replicate 5 (max 6.7 6.9))
m = sum.replicate 5.max 6.7 $ 6.9

n = map (\x -> negate (abs x)) [1, -3, 3]
o = map (negate.abs) [1, -3, 3]


-- DATA TYPES

-- data Shape
--    = Circle Float Float Float
--    | Rectangle Float Float Float Float
--   deriving (Show)
-- 
-- surface :: Shape -> Float
-- surface (Circle _ _ r) = pi * r^2
-- surface (Rectangle x1 y1 x2 y2) = (abs $ x1 - x2)*(abs $ y1 - y2)
-- 
-- p = surface $ Circle 1 1 1 

data Point = Point Float Float deriving (Show)
data Shape 
  = Circle Point Float
  | Rectangle Point Point
  deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2 
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2 + a) (y2 + b))


p = surface $ Rectangle (Point 1 1) (Point 2 2)
q = nudge (Circle (Point 1 1) 1) 1 1


-- RECORD SYNTAX
-- more readable, automatic getting functions, parameters don't have to be specified in order. 

data Car = Car {
  company :: String,
  model :: String, 
  year :: Int 
} deriving (Show)

t = Car {year=2014, model="Model 3", company="Tesla"}
u = model t

-- TYPE PARAMETERS
-- we only use this when the type that's contained inside the data type's various value constructors isn't really that important for the type to work
-- Maybe a = Nothing | Just a

data Vector a = Vector a a a deriving (Show)
vplus :: (Num a) => Vector a -> Vector a -> Vector a
vplus (Vector i j k) (Vector l m n) = Vector (i+l) (j+m) (k+n)

v = vplus (Vector 1 1 1) (Vector 1 1 1)

-- DERIVED INSTANCES
-- Type classes such as Eq, Ord, Show, Read, Bounded, Enum have a predefined set of behaviors. A data type be added to these typeclasses by implementing those predefined functions or by using the deriving keyword

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday   
           deriving (Eq, Ord, Show, Read, Bounded, Enum) 

-- TYPE SYNONYMS
-- data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)  

data LockerState = Taken | Free deriving (Show, Eq)


lockerLookup :: Int -> Map.Map Int (LockerState, String) -> Either String String 
lockerLookup lockerNumber map = 
  case Map.lookup lockerNumber map of 
    Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
    Just (a, b) -> if a /= Taken 
                   then Right b
                   else Left ("Locker " ++ show lockerNumber ++ " is already taken!")


lockers = Map.fromList
  [
    (100,(Taken,"ZD39I"))  
   ,(101,(Free,"JAH3I"))  
   ,(103,(Free,"IQSA9"))  
   ,(105,(Free,"QOTSA"))  
   ,(109,(Taken,"893JJ"))  
   ,(110,(Taken,"99292"))
  ]

w =lockerLookup 100 lockers

-- Compile a program
-- 1. ghc --make <program name>
-- 2. navigate to the directory where the program is located and run "./<program name>"

-- IO

-- 1. Use "printStrLn" to print strings to avoid the double quotes. All other types can be printed using "print"


-- main = do   
--     line <- getLine  
--     if null line  
--         then return ()  
--         else do  
--             putStrLn $ reverseWords line  
--             main  
--   
-- reverseWords :: String -> String  
-- reverseWords = unwords . map reverse . words


-- I/O 

-- main = forever $ do -- forever takes an IO action as a parameter and returns an IO action that executes the IO action forever

--  x <- sequence $ map print [1, 2, 3] -- sequence takes a list of I/O actions as input and returns an I/O action that will evaluate those actions one after another. The result of the IO action is a list of the results of all the IO actions that were performed. 
--  y <- mapM print [11, 22, 33] -- mapM takes a function and a list, maps the function over the list and then sequences it.
--  mapM_ print [111, 222, 333] -- the same as mapM except it throws away the result
--
--  return ()  

-- forM is the same as mapM only with its parameters switched around. We normally use it when we want to map and sequence some actions that we define there on the spot using do notation. 
main = do
  colors <- forM [1, 2, 3, 4] (\x -> do
    putStrLn $ "Which color do you associate with the number " ++ show x
    getLine)
  putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
  mapM putStrLn colors

-- ctrl + D is the end of file character
--
-- getContents is lazy and reads a line at a time recursively (like getLine inside forever)

-- import Control.Monad
-- import Data.Char
-- main = do
-- contents <- getContents
-- putStr $ map toUpper contents












