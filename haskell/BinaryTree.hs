data Tree a 
  = Empty
  | Node a (Tree a) (Tree a)
  deriving (Show, Ord, Eq)

build :: (Ord a) => a -> Tree a -> Tree a 
build x Empty = Node x Empty Empty
build x (Node a l r)
  | x == a = Node a l r
  | x < a  = Node a (build x l) r
  | x > a  = Node a l (build x r)

search :: (Ord a, Eq a) => a -> Tree a -> Bool
search x Empty = False
search x (Node a l r)
  | x == a = True
  | x < a  = search x l 
  | x > a  = search x r

nums = [8,6,4,1,7,3,5]
acc = Empty 
tree = foldr build acc nums 
