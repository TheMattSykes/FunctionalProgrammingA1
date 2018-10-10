module Bags where
  
  import Data.List
  
  -- Polymorphic Datatype for Bag
  type Bag a = [(a,Int)]
  
  
  -- listToBag Function
  listToBag :: Ord a => [a] -> Bag a
  listToBag [x] = [(x,1)]
  listToBag list
    | x /= xs = [(x,1)]
    | otherwise = [(x, 1)] ++ listToBag xss
    where (x:xs:xss) = list
    
  -- zip list of compressed elements with counts
  
  
  -- bagEqual Function
  bagEqual :: Ord a => Bag a -> Bag a -> Bool
  bagEqual [] [] = error "Bags are empty"
  bagEqual bag1 bag2
    | length bag1 /= length bag2 = False -- If not equal length than not equal
    | ((length bag1) == 1) && ((length bag2) == 1) = (x == y) -- On last item check match, base case
    | otherwise = x == y && bagEqual xs ys -- If list elements equal next elements, then lists equal
    where (x:xs) = bag1 -- prevents pattern error on pass of []
          (y:ys) = bag2
  
  
  -- bagInsert Function
  bagInsert :: Ord a => Bag a -> a -> Bag a
  bagInsert [] newItem = error "Bag is empty"
  bagInsert bag newItem
    | length bag == 1 = bag ++ [(newItem, 1)] -- end of bag, append item to end
    | newItem == fst (head xs) = bag ++ [(newItem,((snd (head xs)) + 1))]
    | otherwise = bagInsert xs newItem -- recursive call
    where (x:xs) = bag -- prevents pattern error on pass of [], split the list into head and tail
  
  
  -- bagSum Function
  bagSum :: Ord a => Bag a -> Int
  bagSum [] = error "Bag is empty"
  bagSum bag
    | length bag == 1 = snd x -- return second part of tuple, base case
    | length bag >= 1 = snd x + bagSum xs -- add second part of tuple to recursive call result
    | otherwise = 0
    where (x:xs) = bag -- prevents pattern error on pass of [], split the list into head and tail
    

  -- bagIntersection Function
  bagIntersection :: Ord a => Bag a -> Bag a -> Int
  bagIntersection [] [] = error "Bags are empty"
  bagIntersection bag1 bag2
    | bagEqual bag1 bag2 = min (bagSum bag1) (bagSum bag2) -- Checks if equal then returns lowest sum of bag items
    | otherwise = error "Bags not equal"
    where (x:xs) = bag1 -- prevents pattern error on pass of []
          (y:ys) = bag2