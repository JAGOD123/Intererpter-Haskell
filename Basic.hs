 
------------------------------------------------------------------- 
-- HASKELL BASIC LIST AND STRING PROCESSING FUNCTIONS           
-- Roy L. Crole and Paula Severi 2024                                         
------------------------------------------------------------------- 

module Basic where

mem :: Eq a => a -> [a] -> Bool
mem _ [] = False
mem x (y:ys) | x == y = True
             | otherwise = mem x ys


lookUp :: Eq a => [(a,b)] -> a -> b
lookUp [] _ = error "Empty State"
lookUp ((k, v):rs) x | k == x = v
                     | otherwise = lookUp rs x 


update :: Eq a => [(a, b)] -> a -> b -> [(a, b)]
update [] k v = [(k, v)]
update ((k', v'):xs) k v 
    | k' == k   = (k, v): xs 
    | otherwise = (k', v') : update xs k v 
       

