import Data.List
import Data.Char





-- minden számot egy listában cseréljünk ki egy listával, amely annyiszor tartalmazza önmagát, amennyi
transform :: [Int] -> [[Int]]
transform [] = []
transform (x:xs) = take x (repeat x) : transform xs

--map
map' :: ( a -> b)-> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs


-- isEvens' l = map' (\ x -> even x) l
isEvens :: [Int] -> [Bool]
isEvens l = map' even l

transform' :: [Int] -> [[Int]]
transform' l = map' (\ x -> take x (repeat x)) l
-- upperToLower (nagybetűket alakítsuk kisbetűvé, és csak ezeket adjuk vissza)

upperToLower :: String -> String
upperToLower l = map' toLower $ filter isUpper l

-- upperToLower s = map' toLower $ filter isUpper s


-- max

-- and

-- foldr
-- hajtogatást tesz lehetővé, pl. max, sum, and, or, stb.
-- A lista elemeit "összeműveletezi" (pl. összeadja, össze"és"elni, stb.). A művelet lesz az f paraméter
-- ehhez meg kell adnunk egy kezdeti értéket (d), amit az üres lista esetén akarunk visszaadni

-- sum' (x:xs) = x + sum' xs

