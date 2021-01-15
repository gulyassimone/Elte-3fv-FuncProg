sum' :: [Int] -> Int
sum' []     = 0
sum' (x:xs) = x + sum' xs

and' :: [Bool] -> Bool
and' []     = True
and' (x:xs) = x && and' xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f d []     = d
foldr' f d (x:xs) = f x (foldr' f d xs)

sum'' :: [Int] -> Int
sum'' = foldr' (+) 0

and'' ::[Bool] -> Bool
and'' = foldr' (&&) True

length' ::[a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

length'' :: [a] -> Int
length'' = foldr' (\ x recx -> recx + 1+1) 0

{-
foldl2' :: (b-> a -> b) -> b -> [a] -> b
foldl2' f d []     = d
foldl2' f d (x:xs) = f (foldl2 f d xs) x
-}

max' :: Ord a => [a] -> a
max' [] = undefined
max' [x] = x
max' (x:xs) = max x (max' xs)


max'' :: Ord a => [a] -> a
max'' (x:xs) = foldr' max x xs

orList :: [Bool] -> Bool
orList = foldr' (||) False

anyList :: (a -> Bool) -> [a] -> Bool
anyList a b = foldr || True b 