module Bags where
  
  -- Polymorphic Datatype for Bag
  type Bag a = [(a,Int)]
  
  
  
  -- listToBag Function
  listToBag :: Ord a => [a] -> Bag a
  listToBag [] = error "Entered list is empty" -- Returns error for empty lists
  listToBag list = bagConverter list [] -- Call aux function and append result to empty list (bag)
  
  -- bagConverter Function (aux)
  -- Takes a list and an empty bag then compresses recurring elements into a list of tuples: (item, amount)
  -- Uses bagInsert to accomplish this: If item not in the bag then append to the end, if element exists add 1 to it's amount.
  bagConverter :: Ord a => [a] -> Bag a -> Bag a
  bagConverter list newBag
    | length list == 1 = bagInsert newBag x -- Base case, insert last element
    | otherwise = bagConverter xs ( bagInsert newBag x ) -- Recursive call and insert of item
    where (x:xs) = list -- list split into head and tail, list variable used to refer to both
  
  
  
  -- bagEqual Function
  bagEqual :: Ord a => Bag a -> Bag a -> Bool
  bagEqual [] [] = True -- Bags equal if two empty bags provided
  bagEqual bag1 bag2
    | length bag1 /= length bag2 = False -- If lengths not equal then bags not equal
    | otherwise = equalCheck bag1 bag2 bag1 -- Use aux function equalCheck
          
  -- equalCheck Function (aux)
  -- Takes two bags and the tail of a bag.
  -- Recursively checks whether the head of the first bag is in the second bag.
  equalCheck :: Ord a => Bag a -> Bag a -> Bag a -> Bool
  equalCheck bag1 bag2 subBag
    | ((length subBag) == 1) = x `elem` bag2 -- On last item check match, base case
    | otherwise = x `elem` bag2 && equalCheck bag1 bag2 xs -- Checks whether item in bag then performs AND operation on recursive call.
    where (x:xs) = subBag
  
  
  
  -- bagInsert Function
  bagInsert :: Ord a => Bag a -> a -> Bag a
  bagInsert [] newItem = [(newItem,1)] -- Add item to empty list
  bagInsert bag newItem = inserter bag newItem 1 [] False -- Use aux function
  
  -- inserter Function (aux)
  -- Takes a bag, a new item, it's count, an empty list and a Bool (whether the item exists).
  -- Function auxiliary for both bagEqual and bagSum.
  -- Recursively searches the bag to see if the item exists, appends it's count if found. Otherwise, adds item to end of list.
  inserter :: Ord a => Bag a -> a -> Int -> Bag a -> Bool -> Bag a
  inserter bag newItem numberOfItem newList found
    | (length bag == 0) = if found then newList else newList ++ [(newItem,numberOfItem)] -- Base case, returns non appended list if found, otherwise adds item.
    | newItem == fst (x) = inserter (xs) newItem numberOfItem (newList ++ [(newItem, (snd (x))+numberOfItem)]) True -- If item exists then add 1 to snd of tuple
    | otherwise = inserter xs newItem numberOfItem (newList ++ [x]) found -- Recursive call with new list appending existing bag elements.
    where (x:xs) = bag
  
  
  
  -- BagSum Function
  -- Takes two bags. Inserts elements from second bag into first bag using the aux Inserter function.
  bagSum :: Ord a => Bag a -> Bag a -> Bag a
  bagSum [] [] = [] -- Returns empty bag if empty bags entered
  bagSum bag1 [] = bag1 -- Second bag empty returns first bag as sum, prevents error
  bagSum bag1 bag2
    | length bag2 == 1 = inserter bag1 (fst x) (snd x) [] False -- Base case, returns bag with inserted second element
    | otherwise = bagSum (inserter bag1 (fst x) (snd x) [] False) xs -- Recrusive call, bag1 contains new elements, bag2 tail.
    where (x:xs) = bag2
  
  
  
  -- bagIntersection Function
  bagIntersection :: Ord a => Bag a -> Bag a -> Bag a
  bagIntersection [] [] = error "Bags are empty" -- Intersection controlled error if empty bags entered.
  bagIntersection bag1 bag2 = intersectionA bag1 bag2 [] -- Uses aux function
  
  -- intersectionA (aux)
  -- Takes two bags and an empty bag (initially).
  -- Recursively checks if the first tuple element is contained within a list of first tuple elements. E.g. [("A",1)] is ["A"].
  -- The function then checks the minimum item count between the first and second bags.
  intersectionA :: Ord a => Bag a -> Bag a -> Bag a -> Bag a
  intersectionA bag1 bag2 intersectBag
    | length bag1 == 0 = intersectBag -- End of bag, base case
    | (fst x) `elem` (reduceBag bag2 []) = intersectionA xs bag2 (intersectBag ++ [(fst x, min (snd x) (findItemAmount bag2 (fst x)))])
    | otherwise = intersectionA xs bag2 intersectBag
    where (x:xs) = bag1
  
  -- findItemAmount (aux)
  -- Takes a bag and an item. Returns the second tuple element corresponding to the first tuple element matching the item.
  findItemAmount :: Ord a => Bag a -> a -> Int
  findItemAmount bag itemToFind
    | length bag == 0 = error "Item not found" -- Item not found when end of bag reached
    | fst x == itemToFind = snd x -- Returns value when found
    | otherwise = findItemAmount xs itemToFind -- Recursive call until found
    where (x:xs) = bag
  
  -- reduceBag (aux)
  -- Does the opposite of listToBag. When a bag is entered it will be compressed to it's fst tuple elements only, item counts are ignored.
  reduceBag :: Ord a => Bag a -> [a] -> [a]
  reduceBag bag newList
    | length bag == 0 = newList -- Base case, end of list
    | otherwise = reduceBag xs (newList ++ [fst x])
    where (x:xs) = bag
          