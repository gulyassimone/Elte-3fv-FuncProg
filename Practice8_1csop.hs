-- PLAGIUMELLENORZES
-- NINCS HATARIDO-KITOLAS
-- hatékonyság: squarenums 2-féleképpen
import Data.List
import Data.Char

squarenums :: [Integer] -> [Integer]
-- squarenums l = filter 
    -- (\ x -> elem x (take 10000 [ n ^ 2 | n <- [1..] ])) l
squarenums l = filter cond l where
  cond :: Integer -> Bool
  cond x = elem x squares

squares :: [Integer]
squares = take 10000 [ n ^ 2 | n <- [1..] ]

-- isEvens

isEvens :: [Int] -> [Bool]
isEvens []     = []
isEvens (x:xs) = even x : isEvens xs

-- minden számot egy listában cseréljünk ki egy listával, amely annyiszor tartalmazza önmagát, amennyi

transform :: [Int] -> [[Int]]
transform []     = []
transform (x:xs) = take x (repeat x) : transform xs

-- map
map' :: (a -> b) -> [a] -> [b]
map' _ []     = []
map' f (x:xs) = f x : map' f xs

transform' :: [Int] -> [[Int]]
transform' l = map' (\ x -> take x (repeat x)) l

isEvens' l = map' even l
-- isEvens' l = map' (\ x -> even x) l

-- upperToLower (nagybetűket alakítsuk kisbetűvé, és csak ezeket adjuk vissza)

upperToLower :: String -> String
-- upperToLower s = map' toLower $ filter isUpper s
upperToLower s = map' toLower (filter isUpper s)

-- max

-- and

-- foldr
-- hajtogatást tesz lehetővé, pl. max, sum, and, or, stb.
-- A lista elemeit "összeműveletezi" (pl. összeadja, össze"és"elni, stb.). A művelet lesz az f paraméter
-- ehhez meg kell adnunk egy kezdeti értéket (d), amit az üres lista esetén akarunk visszaadni

sum' [] = 0
-- sum' (x:xs) = x + sum' xs
sum' (x:xs) = (+) x (sum' xs)

foldr' f d []     = d
foldr' f d (x:xs) = f x (foldr' f d xs)

