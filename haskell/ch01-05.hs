
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0 = [] 
take' _ [] = []
take' n (x:xs) = x:take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [a] -> [(a,a)]
zip' _ []  = []
zip' [] _  = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' e (x:xs) 
    | e == x = True
    | otherwise = elem' e xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        largerSorted  = quicksort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ largerSorted

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z
