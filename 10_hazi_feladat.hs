--Hozd létre a Season típust, mely évszakokat (Spring, Summer, Autumn, Winter) reprezentál!
data Season = Summer | Autumn | Winter | Spring  deriving ( Eq, Show, Enum, Ord )

--Definiáld a isNextSeason függvényt, amely két évszakot vár paraméterként, és eldönti, hogy az első után következik-e a második (természetesen szokásosan, naptár szerint)!
isNextSeason :: Season -> Season -> Bool
isNextSeason a Summer = a == Spring
isNextSeason a b = a == (pred b) --pred succ


--Definiáld a Shape típus, amely lehetséges értékei: Circle, Rectangle, Triangle! Legyenek a konstruktorok paraméteresek: kör esetében a sugár, téglalap esetében a két oldal és háromszög esetében az alap és a magasság hosszát jelentsék a konstruktorok paraméterei (használjunk Double-t)! Pl. Circle 5.0 : 5.0 sugarú kör.

--Definiáld az area függvényt, amely kiszámítja egy alakzat területét!

data Shape = Circle Double  | Rectangle Double Double | Triangle Double Double 

area :: Shape -> Double
area (Circle a) = a * 3 
area (Rectangle a b) = a*b
area (Triangle a b) =  (a*b) / 2
--Okosítsd fel a gyakorlaton vett Time típust. Adott:

data Time = T Int Int 

instance Show (Time) where ---elem eq vizsgálatát használja az elem
    show (T a b) = show a ++ "." ++ show b


instance Eq (Time) where ---elem eq vizsgálatát használja az elem
    T a1 a2 == T b1 b2 = a1 == a2 && b1 == b2

instance Ord (Time) where ---elem eq vizsgálatát használja az elem
    T a1 a2 <= T b1 b2 = a1 <= a2 || b1 <= b2


--Definiálj egyenlőségvizsgálatot (eqTime)!
eqTime :: Time -> Time -> Bool
eqTime (T a1 a2 ) (T b1 b2) = a1 == a2 && b1 == b2

--Vizsgáld meg, hogy egy időpont korábban van-e, mint egy másik (isEarlier)!
isEarlier :: Time -> Time -> Bool
isEarlier  (T a1 a2 ) (T b1 b2) = a1 < a2 || b1 < b2

--Definiálj egy ú.n. smart constructort időpontokhoz! Ez hibát jelez érvénytelen időpontok esetén. A tesztek lefutásához szükséges az eqTime függvény. Hibák kiírásához használd az error függvényt!

f :: (Eq a1, Eq a2, Num a1, Num a2) => Either [a1] a2 -> Maybe Bool
f  (Left [1,2,3]) = Just True 
f(Right 2) = Just False 
f _ = Nothing 

