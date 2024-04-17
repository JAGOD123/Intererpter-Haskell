 
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
lookUp ((key, val):rs) x | key == x = val
                     | otherwise = lookUp rs x 


update :: Eq a => [(a, b)] -> a -> b -> [(a, b)]
update [] key val = [(key, val)]
update ((key', val'):xs) key val 
    | key' == key   = (key, val): xs 
    | otherwise = (key', val') : update xs key val 
       

