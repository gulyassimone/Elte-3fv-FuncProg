import Data.Char
import Data.List

--isPrefixOf
isPrefixOf' :: [Char] -> [Char] -> Bool
isPrefixOf' [] _ = True
isPrefixOf' _ [] = False
isPrefixOf' (x:xs) (y:ys) = x == y && isPrefixOf' xs ys

--evens
evens :: [Int] -> [Int]
evens []     = []
evens (x:xs) = let res = evens xs in
                 if even x
                 then x : res
                 else res

--elso 100 db 3 hatányban melyek szerepelnek 


--filter
--is evens

--minden számot egy listában cseréljünk ki egy listával, amely annyiszor tartalmazza önmagát, amennyi 


--map

--uppertoLower mappal (nagybetűket alakítsuk kisbetűvé és csak ezeket adjuk vissza

--max

--and 