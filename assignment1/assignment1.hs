myLast :: [a] -> a
myLast x = last x

--myReverse :: [a] -> [a]
myReverse [x] = [x] 
myReverse (x:xs) = (myReverse xs) ++ [x]

--isPalindrome :: Eq a => [a] -> [a]
isPalindrome x = x == (myReverse x)

compress :: Eq a => [a] -> [a]
compress a
    | (xs == []) = [x]
    | (x == head xs) = [] ++ compress xs
    | otherwise = [x] ++ compress xs
    where x = head a
          xs = tail a

duplicate :: [a] -> [a]
duplicate [] = []
duplicate (x:xs) = [x,x] ++ duplicate xs

rotate :: [a]->Int->[a]
rotate a n
    | n == 0 = a
    | otherwise = (rotate (tail a) (n-1) ) ++ [head a]

insertAt :: a -> [a] -> Int -> [a]
insertAt a b n
    | n == 1 = [a] ++ b
    | otherwise = [head b] ++ insertAt a (tail b) (n - 1) 

--code to make combinations of 3
--combinations :: (Eq a) => Int -> [a] -> [[a]]
--combinations n a =[[x,y,z] | x <- a , y <- a, z <- a , (x /= y) && (y /= z) && (x /= z)]

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = [xs !! index : x | index <- [0..(length xs)-1] , x <- combinations (n - 1) (drop (index + 1) xs) ]


isPrime :: Int -> Bool
isPrime k = null [ x | x <- [2..k - 1], k `mod` x == 0]

