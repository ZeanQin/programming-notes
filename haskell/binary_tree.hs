data Tree
  = Empty
  | Node Tree Double Tree
  deriving (Show)

add :: Double -> Tree -> Tree
add elt Empty = Node Empty elt Empty
add elt (Node l value r)
  | elt == value = Node l value r
  | elt < value  = Node (add elt l) value r
  | otherwise    = Node l value (add elt r)

count_nodes :: Num a => Tree -> a
count_nodes Empty = 0
count_nodes (Node l value r) = 
  1 + count_nodes l + count_nodes r
  
sum_nodes :: Tree -> Double
sum_nodes Empty = 0.0
sum_nodes (Node l value r) = 
  value + sum_nodes l + sum_nodes r

flatten :: Tree -> [Double]
flatten Empty = []
flatten (Node l value r) = 
  [value] ++ flatten l ++ flatten r

average_value :: Tree -> Double
average_value Empty = 0.0
average_value (Node l value r) = 
  sum / count
  where (sum, count) = get_sum_count (Node l value r)

get_sum_count :: Tree -> (Double, Double)
get_sum_count Empty = (0.0, 0.0)
get_sum_count (Node l value r) = (value + lsum + rsum, 1 + lcount + rcount)
  where (lsum, lcount) = get_sum_count l
        (rsum, rcount) = get_sum_count r

average_value1 :: Tree -> Double
average_value1 Empty = 0.0
average_value1 (Node l value r)
  = sum nodes / fromIntegral(length nodes)
  where nodes = flatten (Node l value r)

average_value2 :: Tree -> Double
average_value2 Empty = 0.0
average_value2 (Node l value r) = sum / count
  where sum = sum_nodes (Node l value r)
        count = count_nodes (Node l value r)
