-- For more details, see http://learnyouahaskell.com/making-our-own-types-and-typeclasses:w

data TrafficLight = Red | Green | Yellow

instance Show TrafficLight where
  show Red = "Red Light" 
  show Green = "Gree Light" 
  show Yellow = "Yellow Light" 

a = Red -- a will be "Red Light" 



