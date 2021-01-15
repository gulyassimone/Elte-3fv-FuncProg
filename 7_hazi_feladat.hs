import Data.List
--Az alábbi feladatokat próbáld meg megoldani map és/vagy filter használatával (kivéve: breakOn, splitOn).
--Definiálj egy sucs függvényt, amely egy számlistában lévő mindegyik elemet megnöveli 1-vel!
sucs :: [Integer] -> [Integer]
sucs [] = []
sucs l = map (\ x -> x+1) l

--Definiálj egy hasLongLines nevű függvényt, mely megvizsgálja,hogy egy fájl sorai között van-e legalább 3 szóból álló! Tipp: itt jól jöhet a lines és words standard könyvtárbeli függvények.
hasLongLines::String -> Bool
hasLongLines l = foldr (||) False (map (\ x -> length (words x) >= 3) (lines l))

--Lyonesse ország külögyminisztériuma számon tartja, hogy mely országokból érkezhetnek küldöttségek. Minden országról ismert a hivatalos nyelve (egyszerűség kedvéért ebből országonként csak egy van).
--Írj függvényt, amely megállapítja, hogy Lyonesse mely országok küldöttségét nem tudja fogadni, azaz mely országok hivatalos nyelvét nem beszéli legalább egy tolmács!

trd ( _ , _ , a ) = a


hasNotTranslator :: [(String, String)] -> [(String, String, String)] -> [String]
hasNotTranslator a [] = map fst a
hasNotTranslator a (x:xs) =  hasNotTranslator s xs 
        where s = filter(not . (\ y -> snd y == trd x) ) a

--Definiálj egy (rekurzív) breakOn függvényt, mely egy szövegben egy megadott karakter előtti és utáni darabot visszaadja!
breakOn :: Char -> String -> (String , String)
breakOn _ [] = ("","")
breakOn delimiter list = ((takeWhile (/= delimiter) list), (dropWhile (/= delimiter) list))


--Definiálj egy splitOn függvényt, mely adott karakter mentén darabol egy Stringet! Tipp: jól jöhet a breakOn!
splitOn  :: Char -> String -> [String]
splitOn  _ "" = []
splitOn  delimiter list =
  map (takeWhile (/= delimiter) . tail)
    (filter (isPrefixOf [delimiter])
       (tails
           (delimiter : list)))


--Adott egy szövegrészlet és sorok egy szöveges fájlban sortöréssel ('\n') elválasztva. Sorold fel azokat a sorokat sorszámukkal együtt, melyek egy megadott szövegrészlettel kezdődnek!

--A sorszámozás 1-gyel kezdődjön!
simpleGrep :: String -> String -> [(String, Int)]
simpleGrep a l = filter (\x -> isPrefixOf a (fst x)) (zip (lines l) [1..])