import Data.Char
import Data.List

--Alakíts egy szöveget csupa nagybetűssé! Legyen a függvény neve transform!
transform :: String ->String 
transform  []  = []
transform (x:xs) = toUpper x : transform xs

--Add meg az hoursMinutes függvényt, aminek nincs bemeneti paramétere, és megadja egy listában az összes lehetséges óra-perc párt!
hoursMinutes::[(Integer,Integer)]
hoursMinutes =  [ (n,m) |  n <- [0..23] , m <- [0..59]]

--Add meg a fours függvényt, aminek nincs bemeneti paramétere, és megad egy 20 hosszú listát, melyben az 1, 2, 3, 4 számsorozat ismétlődik!
fours::[Integer]
fours = take 20 $ cycle [1..4]

--Add meg az abcList függvényt, aminek nincs bemeneti paramétere, és visszaadja a sorszámukkal párba állítva az angol ábécé betűit!
abcList::[(Integer, Char)]
abcList = zip [1..] ['a'..'z']

--Add meg a divisors függvényt, amely egy egész számot vár bemenetként, és visszaadja az osztóit!
divisors :: Integer -> [Integer]
divisors x = [ n |  n  <- [1..x], mod x n == 0]

--Add meg az isPrime' függvényt, amely a paraméteréről eldönti, hogy prím-e! Tipp: hasznos lehet a null függvény, amely eldönti egy listáról, hogy üres-e, vagy a length, amely megadja egy lista hosszát.
isPrime'::Integer -> Bool
isPrime' x = length (divisors x) == 2

--Add meg a compress függvényt, amely egy szöveget tömörít úgy, hogy az egymást követő betűk tárolása darabszám-betű párként van megvalósítva! Tesztek:
compress::String->[(Int, Char)]
compress "" = []
compress l = zip (map length $ group l) (map head $ group l)

--Add meg a decompress függvényt, amely egy darabszám-betű párokat tartalmazó listát visszaalakít szöveggé! Tesztek:
decompress :: [(Int, Char)] -> String
decompress [] = ""
decompress l = concat [replicate (fst x) (snd x) | x <- l]