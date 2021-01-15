--Definiáld a mult függvényt, amely összeszoroz két Int-et! Az első számról felteheted, hogy 0-nál nagyobb, vagy egyenlő. Ne használd a szorzás műveletét, rekurzívan, összeadás segítségével oldd meg a feladatot! 
--Tipp: Mennyi az eredmény, ha az első paraméter 0? És ha 1? És ha 1-nél nagyobb?

mult::Int->Int->Int 
mult 0 _ = 0
mult _ 0 = 0
mult 1 a = a
mult a 1 = a
mult a b = b + mult (a-1) b

--(Nehezítés) Definiáld a mult' függvényt, amely összeszoroz két Int-et, de most már nem teheted fel az első számról, hogy nem negatív! Ne használd a szorzás műveletét, rekurzívan,
-- összeadás segítségével oldd meg a feladatot! Tipp: Használhatod az előbb megírt mult és az abs (abszolútérték) függvényt. Miben különbözik az eredmény, ha az első paraméter negatív?
mult'::Int->Int->Int 
mult' a b  
    | a < 0 && b < 0 = mult (abs a) (abs b)
    | a < 0 =  mult b a
    | otherwise = mult a b

--Definiáld a pow függvényt, ami egész számokat hatványoz! Felteheted, hogy a második paramétere pozitív. Ne használd a hatványozás (^, ^^ és **) műveleteket, szorzás segítségével, rekurzívan oldd meg a feladatot!
pow::Integer->Integer->Integer
pow 0 _ = 0
pow _ 0 = 0
pow 1 a = a
pow a 1 = a
pow a b = a * pow a (b-1) 

--Definiáld a zip' függvényt, amely két listát (amikben tetszőleges típusú értékek szerepelhetnek) összefűz párok formájában (ne használd a zip standard librarybeli függvényt)! Tipp: az eredmény listának annyi eleme lesz, amennyi a rövidebb bemeneti listának volt.
zip':: [a]->[b]->[(a,b)]
zip' _ [] = [] 
zip' [] _ = [] 
zip' (x:xs) (y:ys) = (x , y) : zip' xs ys 

--(Nehezebb feladat) Definiáld az unzip' függvényt, ami szétbontja a párok listáit listák párjává! Ne használd a standard unzip függvényt!
unzip':: [(a,b)] -> ([a],[b])
unzip' [] = ([], [])
unzip' l = ([fst x | x <- l] , [snd x | x <- l]) 

--Definiáld a last' függvényt, amely visszaadja egy lista utolsó elemét! Ne használd a last standard függvényt!
last' :: [a] -> a
last' [x] = x
last' (x:xs) = last' xs

--(Nehezebb feladat) Definiáld a maximum' függvényt rekurzívan, amely visszaadja egy lista legnagyobb elemét! Tipp: üres listának mi a maximuma? Mi a maximuma egyelemű listának? Továbbá használhatod a max függvényt a standard libraryből, ami két érték közül visszaadja a nagyobbat.
maximum'::[Integer ] -> Integer 
maximum' []     = error "null"
maximum' [x]    = x
maximum' (x:xs) = max x (maximum' xs)

--expert
maximum'' :: Ord a => [a]{-véges, nemüres-} -> a
maximum'' l = foldr (\ x y -> max x y) (head l) l