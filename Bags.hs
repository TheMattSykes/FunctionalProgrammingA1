module Bags where
  -- Datatype for Bag
  type Bag = [(String,Int)]
  
  -- listToBag Function
  listToBag :: [String] -> Bag
  listToBag (x:xs)
    | null list = [] -- empty list as input
    | length xs == 1 = []
    | x == head xs = listToBag xs
    | otherwise = []
    where list = (x:xs)
            
  -- bagEqual Function
  bagEqual :: Bag -> Bag -> Bool
  bagEqual (x:xs) (y:ys)
    | length bag1 /= length bag2 = False -- If not equal length than not equal
    | (null bag1 || null bag2) = False -- Empty bag
    | ((length bag1) == 1) && ((length bag2) == 1) = (x == y) -- On last item check match, base case
    | otherwise = x == y && bagEqual xs ys -- If list elements equal next elements, then lists equal
    where bag1 = (x:xs)
          bag2 = (y:ys)
            
  -- bagSum Function
  bagSum :: Bag -> Int
  bagSum (x:xs) -- Split the list into head and tail
    | length bag == 1 = snd x -- return second part of tuple, base case
    | length bag >= 1 = snd x + bagSum xs -- add second part of tuple to recursive call result
    | otherwise = 0
    where bag = (x:xs)