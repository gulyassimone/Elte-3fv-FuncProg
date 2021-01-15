--Definiáld az orList függvényt, amely egy listában lévő logikai értékeket össze“vagy”ol!
orList :: [Bool] -> Bool
orList l = foldr (||) False l

--Definiáld a minList függvényt, amely kiválasztja egy listából a legkisebb elemet! Próbáld meg a szignatúráját is kitalálni ennek a függvénynek!
minList::Ord a => [a] -> a
minList l = foldl (\ x y -> min y x) (head l) l


--Definiáld a concatList függvényt, amely egy lisában lévő listákat konkatenálja össze!
concatList :: [[a]] -> [a]
concatList l = foldl (++) [] l

--Definiáld az allList függvényt, amely egy eldönti, hogy egy predikátum teljesül-e egy lista minden elemére
allList :: (a -> Bool) -> [a] -> Bool
allList f l = foldl (&&) True (map f l)

--Add meg egy listában található 3-mal osztható páros számok négyzetösszegét! A függvény neve legyen powSum!
powSum :: [Integer] -> Integer
powSum l = foldr (+) 0 (map (^2)(filter (\x -> mod x 3 == 0 && mod x 2 == 0) l))

--Állítsd elő egy szavakat (String) tartalmazó listából a reprezentélt mondatot! A függvény neve legyen˛toSentence! (Tipp: removeLast segíthet az eredmény utolsó karakterének levágásában.)
removeLast :: [a] -> [a]
removeLast [] = []
removeLast [x] = []
removeLast (x:xs) = x : removeLast xs

toSentence :: [String] -> String
toSentence l = removeLast (foldl (++) [] (map (++ " ") l))