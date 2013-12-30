
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

add2 :: Int -> Int
add2 x = x + 2

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)  
    | p x = x : filter' p xs
    | otherwise = filter' p xs

largestDivisable :: (Integral a) => a
largestDivisable = head (filter p [1000000,999999..])
    where p x = x `mod` 3829 == 0 

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n 
    | even n = n:chain(n `div` 2)
    | odd  n = n:chain(n*3 + 1)


longChains :: Int
longChains = length (filter isLong (map chain [1..1000]))
    where isLong xs = length xs > 50

sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0 

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' _ acc [] = acc
foldl' f acc (x:xs) = foldl' f (f acc x) xs