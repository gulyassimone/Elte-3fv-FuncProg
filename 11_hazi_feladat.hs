import Data.List
type Stack a = [a]

--Definiáld a push műveletet, amely egy értéket betesz a verem tetejére!
push :: a -> Stack a -> Stack a
push a [] = [a]
push a b =  a : b

--Definiáld a top műveletet, amely visszaadja a verem tetején lévő elemet! Vigyázz, üres verem esetén ne dobj kivételt, hanem használj Maybe típust! Erre a feladatra lehet úgy is gondolni, mint egy biztonságos head függvényre.
top :: Stack a -> Maybe a
top [] = Nothing 
top (x:xs) = Just x

--Definiáld a pop műveletet, amely kiveszi a verem tetején lévő elemet, és visszaadja a kisebb vermet! Vigyázz, üres verem esetén ne dobj kivételt, hanem használj Maybe típust! Erre a feladatra lehet úgy is gondolni, mint egy biztonságos tail függvényre.
pop :: Stack a -> Maybe (Stack a)
pop [] = Nothing 
pop a = Just (tail a) 

--Nehezítés: Az előbbi feladatokat oldd meg úgy is (push nem változik), hogy hibaüzetet is adj vissza az üres vermek esetén ("Error: empty stack")! Ehhez használj Either típust, és a Right konstruktort használd hibajelzésre!
top2 :: Stack a -> Either a String
top2 [] = Right "Error: empty stack"
top2 (x:xs) = Left x

pop2 :: Stack a -> Either (Stack a) String
pop2 [] = Right "Error: empty stack"
pop2 a = Left (tail a) 

--Haladóbb gyakorló feladat (nincs tesztelve): definiáld a verem típust úgy, hogy legyen felső korlátja! Ennél a korlátnál több elemet nem tartalmazhat a verem (azaz mostantól a push művelet is adhat hibát)!

--Add meg a biztonságos lista indexelő függvényt (at) Maybe típussal! Alul, vagy túlindexelés esetén adjunk hibát (Nothing)!

at :: [a] -> Int -> Maybe a
[] `at` a = Nothing 
(x:xs) `at` a
    | length (x:xs)< a = Nothing 
    | a < 0 = Nothing 
    | a == 0       = Just x
    | otherwise    = xs `at` (a-1)

--Trükkösebb feladat: Add meg az előbbi függvényt úgy, hogy hibaüzenetet is adjunk vissza, amely tartalmazza az eredeti indexet, amivel a függvényt meghívták (lásd tesztek)! Számot kiírni a show függvénnyel lehet.

at2 :: [a] -> Int -> Either a String
[] `at2` a =  Right ("Error: invalid index: " ++ show a)
(x:xs) `at2` a 
    | length (x:xs)< a = Right ("Error: invalid index: " ++ show a)
    | a < 0            = Right ("Error: invalid index: " ++ show a)
    | a == 0           = Left x
    | otherwise        = xs `at2` (a-1)