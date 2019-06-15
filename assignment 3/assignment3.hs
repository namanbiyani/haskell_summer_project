sumsql :: (Num a, Enum a) => a -> a
sumsql n = foldl (\acc x -> acc + x^2) 0 [1..n]

sumsqr :: (Num a, Enum a) => a -> a
sumsqr n = foldr (\acc x -> acc + x^2) 0 [1..n]
  
mapr :: (a -> b) -> [a] -> [b]  
mapr f xs = foldr (\x acc -> f x : acc) [] xs 

filterr :: (a -> Bool) -> [a] -> [a]  
filterr p = foldr (\x acc -> if p x then x : acc else acc) []  

reversel :: [a] -> [a]  
reversel = foldl (\acc x -> x : acc) []  

inits a = foldr(\x acc-> [] : map (x:) acc) [[]] a

remdups a = reverse $ foldl (\acc x -> if (acc == [] || x /= head acc) then x:acc else acc) [] a