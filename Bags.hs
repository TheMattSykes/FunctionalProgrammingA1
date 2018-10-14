module Bags where
  
  import Data.List
  
  -- Polymorphic Datatype for Bag
  type Bag a = [(a,Int)]
  
  
  
  -- listToBag Function
  listToBag :: Ord a => [a] -> Bag a
  listToBag [] = error "Empty list"
  listToBag list = bagConverter list []
  
  bagConverter :: Ord a => [a] -> Bag a -> Bag a
  bagConverter list newBag
    | length list == 1 = bagInsert newBag x
    | otherwise = bagConverter xs ( bagInsert newBag x )
    where (x:xs) = list
  
  
  
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
  bagInsert [] newItem = [(newItem,1)]
  bagInsert bag newItem = inserter bag newItem 1 [] False
  
  inserter :: Ord a => Bag a -> a -> Int -> Bag a -> Bool -> Bag a
  inserter bag newItem numberOfItem newList found
    | (length bag == 0) && found = newList
    | (length bag == 0) && not found = newList ++ [(newItem,numberOfItem)]
    | newItem == fst (x) = inserter (xs) newItem numberOfItem (newList ++ [(newItem, (snd (x))+numberOfItem)]) True -- If item exists then add 1 to snd part
    | otherwise = inserter xs newItem numberOfItem (newList ++ [x]) found
    where (x:xs) = bag
  
  
  
  bagSum :: Ord a => Bag a -> Bag a -> Bag a
  bagSum [] [] = []
  bagSum bag1 bag2
    | length bag2 == 1 = inserter bag1 (fst x) (snd x) [] False
    | otherwise = bagSum (inserter bag1 (fst x) (snd x) [] False) xs
    where (x:xs) = bag2
    
          
          
  reduceBag :: Ord a => Bag a -> [a] -> [a]
  reduceBag bag newList
    | length bag == 0 = newList
    | otherwise = reduceBag xs (newList ++ [fst x])
    where (x:xs) = bag
    
  
  intersectChecker :: Ord a => Bag a -> Bag a -> [a] -> [a] -> [a]
  intersectChecker bag1 bag2 intersectList newList
    | otherwise = intersect (reduceBag bag1 []) (reduceBag bag2 [])
    where (x:xs) = bag1 -- prevents pattern error on pass of []
          (y:ys) = bag2
          