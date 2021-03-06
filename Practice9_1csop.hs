import Data.Char
import Data.List


-- filter
-- map

-- sum

sum' :: [Int] -> Int
sum' []     = 0
sum' (x:xs) = (+) x (sum' xs)

-- and
and' :: [Bool] -> Bool
and' []     = True
and' (x:xs) = (&&) x (and' xs)

-- foldr
-- hajtogatást tesz lehetővé, pl. max, sum, and, or, stb.
-- A lista elemeit "összeműveletezi" (pl. összeadja, össze"és"elni, stb.). A művelet lesz az f paraméter
-- ehhez meg kell adnunk egy kezdeti értéket (d), amit az üres lista esetén akarunk visszaadni

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f d []     = d
foldr' f d (x:xs) = f x (foldr' f d xs)

{-
foldl2 :: (b -> a -> b) -> b -> [a] -> b
foldl2 f d []     = d
foldl2 f d (x:xs) = f (foldl2 f d xs) x
-}

sum'' :: [Int] -> Int
sum'' = foldr' (+) 0

and'' :: [Bool] -> Bool
and'' = foldr' (&&) True

-- max

-- length
length' :: [a] -> Int
length' []     = 0
length' (_:xs) = (+) 1 (length' xs)

length'' :: [a] -> Int
length'' = foldr' (\ _ recxs -> 1 + recxs ) 0

-- Minden számot szorozz meg 2-vel egy listában, majd szűrd ki ezek közül a 3-mal oszthatókat

max' :: Ord a => [a] -> a
max' []     = undefined
max' [x]    = x
max' (x:xs) = max x (max' xs)

max'' (x:xs) = foldr' max x xs

-- Definiálj egy `hasLongLines` nevű függvényt, mely megvizsgálja,hogy egy fájl sorai között van-e legalább 3 szóból álló! Tipp: itt jól jöhet a `lines` és `words` standard könyvtárbeli függvények.
