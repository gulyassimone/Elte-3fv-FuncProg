
import Data.Char

-- String = [Char]
changeHd :: String -> Char
changeHd []     = '?'
changeHd (x:xs) = if isLower x then toUpper x else toLower x

-- Számjegy-e a karakter paramétere egy függvénynek?
isDigit' :: Char -> Bool
isDigit' c = elem c ['0'..'9']

-- Betű-e a paramétere egy függvénynek?
isLetter' :: Char -> Bool
-- isLetter' c = elem c (['a'..'z'] ++ ['A'..'Z'])
isLetter' c = elem (toLower c) ['a'..'z']

-- Három hatványai (első 20)
threePowers :: [Integer]
threePowers = take 20 [ 3 ^ n | n <- [0..] ]
--threePowers = [ 3 ^ n | n <- [0..] ]


-- Tíz hosszú lista, True és False felváltva
truesFalses :: [Integer]
truesFalses = [ n `mod` 2 == 0 | n <- [0..9] ]


-- Melyik az az első három hatvány nagyobb, mint 10^20?
first :: Integer
first = head [ 3 ^ n | n <- [0..], 3 ^ n > 10 ^ 20 ]

-- Alakíts egy szöveget csupa nagybetűssé!



-- Prímszám-e a paraméter?



-- Óra perc párok



-- Állítsuk elő a [(1, 'a'), (2, 'b'), .. , (..., 'z')] listát


