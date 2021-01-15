--Definiálj egy hasLongLines nevű függvényt, mely megvizsgálja,hogy egy fájl sorai között van-e legalább 3 szóból álló! Tipp: itt jól jöhet a lines és words standard könyvtárbeli függvények.
hasLongLines :: String -> Bool
--hasLongLines l = foldr (||) False (map (((>= 3) . length ) . words)  (lines l))
hasLongLines = not . null . filter (>2) . map (length  . words)  . lines 

--Add meg egy listában található 3-mal osztható páros számok négyzetösszegét! A függvény neve legyen powSum!
powSum :: [Integer] -> Integer
powSum l = foldr (+) 0 (map (^2)(filter (\x -> mod x 3 == 0 && mod x 2 == 0) l))

--Adott elemek listájának listája. Keressük meg a legnagyobbat a legkisebb elemek közül! Próbáld meg függvénykompozícióval (is) megoldani!
--maximumOfMinimums :: Ord a => [[a]] -> a
--maximumOfMinimums = 