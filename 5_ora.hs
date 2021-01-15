import Data.Char
-- String = [Char]
changeHd :: String -> Char
changeHd[] = '?'
changeHd (x:xs) = if isLower x then toUpper x else toLower x

--Számjegy-e a karakter paramétere egy függvénynek? 
isDigit' :: Char -> Bool
isDigit' c = elem c ['0'..'9']

-- Betűk-e a paramétere egy függvénynek
isLetter' :: Char -> Bool
isLetter' c = elem c (['a'..'z'] ++ ['A'..'Z'])

--Három hatványai
--[ n | n <-[0..100], mod n 2 == 0 ] - 2 hatványai 0..100 között
-- [ 3 ^ n | n <- [0..19]]
threePowers :: [Integer]
threePowers = take 20 [ 3 ^ n | n <- [0..] ]

--Tiz hosszú lista True és False felváltva
truesFalses :: [Bool]
truesFalses = [ n `mod` 2 == 0 | n <- [0..9] ]

--Melyik az az első három hatvány, amelyik nagyobb mint 10^20
first:: Integer
first = head [ 3 ^n | n <- [0..], 3 ^ n > 10 ^20 ]