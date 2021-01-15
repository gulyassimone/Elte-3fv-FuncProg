-- Beadandó: plágiumellenőrzés!
-- határidő: nov. 15.

import Data.Char
import Data.List

-- isPrefixOf
isPrefixOf' :: [Char] -> [Char] -> Bool
isPrefixOf' []     _      = True
isPrefixOf' _      []     = False
isPrefixOf' (x:xs) (y:ys) = x == y && isPrefixOf' xs ys

-- evens

evens :: [Int] -> [Int]
evens []     = []
evens (x:xs) = let res = evens xs in
                 if even x
                 then x : res
                 else res

-- első 100 db 3 hatványban melyek szerepelnek
powersOfThree :: [Integer]
powersOfThree = take 100 [ 3 ^ n | n <- [0..] ]

firstn :: [Integer] -> [Integer]
firstn []     = []
firstn (x:xs) = let res = firstn xs in
                   if elem x powersOfThree
                   then x : res
                   else res

-- filter
filter' :: (a -> Bool) -> [a] -> [a] 
filter' cond [] = []
filter' cond (x:xs) = let res = filter' cond xs in
                         if cond x
                         then x : res
                         else res

evens' l = filter' even l
evens'' = filter' even
evens''' l = filter' (\ x -> x `mod` 2 == 0) l
evens'''' l = filter' even' l where
   even' x = x `mod` 2 == 0

-- isEvens


-- minden számot egy listában cseréljünk ki egy listával, amely annyiszor tartalmazza önmagát, amennyi

-- map

-- upperToLower mappal (nagybetűket alakítsuk kisbetűvé, és csak ezeket adjuk vissza)

-- max

-- and

-- foldr
odds :: [Int] -> [Int]
odds []     = []
odds (x:xs) = let res = odds xs in
                 if even x
                 then res
                 else x : res

negyzet :: [Integer]
negyzet = take 10000 [ n ^ 2 | n <- [1..] ]

squarenums :: [Integer] -> [Integer]
squarenums []     = []
squarenums (x:xs) = let res = squarenums xs in
                   if elem x negyzet
                   then x : res
                   else res