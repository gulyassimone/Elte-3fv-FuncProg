import Data.Char
--Add meg az isSingletonInt függvényt, amely megadja, hogy egy Integer lista egy elemű-e!
isSingletonInt::[Integer]->Bool 
isSingletonInt a =length (take 2 a) == 1

--Definiálj egy toUpperFirst függvényt, mely egy szöveg (String = [Char]) első betűjét nagybetűre cseréli! Üres szöveget változatlanul adja vissza a függvény! (Tipp: használd a toUpper függvényt a Data.Char modulból)
toUpperFirst::[Char] ->  [Char]
toUpperFirst [] = ""
toUpperFirst (x:xs) = toUpper x : xs

--Definiálj egy changeFirst függvényt, amely egy szöveg első betűjét nagybetűre cseréli, ha kisbetű volt, és kisbetűre, ha nagybetű volt! Üres szöveget változatlanul adja vissza a függvény!
changeFirst::[Char] ->  [Char]
changeFirst [] = ""
changeFirst (x:xs) = if isUpper x then toLower x : xs else toUpper x : xs
