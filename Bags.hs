module Bags where
  
  import Data.List
  
  -- Polymorphic Datatype for Bag
  type Bag a = [(a,Int)]
  
  
  
  -- listToBag Function
  listToBag :: Ord a => [a] -> Bag a
  listToBag [] = error "Empty list"
  listToBag list = bagConverter list [] 1
  
  bagConverter :: Ord a => [a] -> Bag a -> Int -> Bag a
  bagConverter list bag count
    | (not (null xs)) && (x /= head xs) && (length (x:xs) >= 1) = bagConverter xs (bag ++ [(x,count)]) 1
    | length list == 1 = bag ++ [(x,count)]
    | otherwise = bagConverter xs bag (count + 1)
    where (x:xs) = sort list
  
  
  
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
  bagInsert [] _ = error "Bag is empty"
  bagInsert bag newItem = inserter bag newItem [] False
  
  inserter :: Ord a => Bag a -> a -> Bag a -> Bool -> Bag a
  inserter bag newItem newList found
    | (length bag == 0) && found = newList
    | (length bag == 0) && not found = newList ++ [(newItem,1)]
    | newItem == fst (x) = inserter (xs) newItem (newList ++ [(newItem, (snd (x))+1)]) True -- If item exists then add 1 to snd part
    | otherwise = inserter xs newItem (newList ++ [x]) found
    where (x:xs) = bag
  
  
  
  
  bagSum :: Ord a => Bag a -> Bag a -> Bag a -> Bag a
  bagSum [] [] newBag = newBag
  bagSum bag1 bag2 newBag
    | (fst x) == (fst y) = bagSum xs ys (newBag ++ [(fst x,(snd x + snd y))])
    | otherwise = bagSum xs ys (newBag ++ [(fst x,snd x)] ++ [(fst y,snd y)])
    where (x:xs) = bag1
          (y:ys) = bag2
  
  -- bagSum Function
  -- bagSum :: Ord a => Bag a -> Int
  -- bagSum [] = 0
  -- bagSum bag
  --   | length bag == 1 = snd x -- return second part of tuple, base case
  --   | length bag >= 1 = snd x + bagSum xs -- add second part of tuple to recursive call result
  --   | otherwise = 0
  --   where (x:xs) = bag -- prevents pattern error on pass of [], split the list into head and tail
    
  
  
  -- bagIntersection Function
  -- bagIntersection :: Ord a => Bag a -> Bag a -> Int
  -- bagIntersection [] [] = error "Bags are empty"
  -- bagIntersection bag1 bag2
  --   | bagEqual bag1 bag2 = min (bagSum bag1) (bagSum bag2) -- Checks if equal then returns lowest sum of bag items
  --   | otherwise = error "Bags not equal"
  --   where (x:xs) = bag1 -- prevents pattern error on pass of []
  --         (y:ys) = bag2
          
          
  reduceBag :: Ord a => Bag a -> [a] -> [a]
  reduceBag bag newList
    | length bag == 0 = newList
    | otherwise = reduceBag xs (newList ++ [fst x])
    where (x:xs) = bag
    
    
  -- bagOfIntersects :: Ord a => Bag a -> [a] -> Bag a -> Bag a
  -- bagOfIntersects bag intersectList newBag
  --   | length x == 0 = newBag
  --   | fst x == (inspectList y) = bagOfIntersects xs intersectList (newBag ++ [y])
  --   | otherwise = bagOfIntersects xs intersectList newBag
  --   where (x:xs) = bag
  --         (y:ys) = intersectList
    
  
  intersectChecker :: Ord a => Bag a -> Bag a -> [a] -> [a] -> [a]
  intersectChecker bag1 bag2 intersectList newList
    | otherwise = intersect (reduceBag bag1 []) (reduceBag bag2 [])
    where (x:xs) = bag1 -- prevents pattern error on pass of []
          (y:ys) = bag2