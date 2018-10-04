module Bags where
  -- Datatype for Bag
  type Bag = [(String,Int)]
  
  -- listToBag Function
  listToBag :: [String] -> Bag
  listToBag list
    | null list = [] -- empty list as input
    | (head list /= head (tail list)) = (: (head list,length list))
    | (head list == head (tail list)) = listToBag (tail list)