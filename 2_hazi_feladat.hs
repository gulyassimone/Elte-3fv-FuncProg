--Definiáld az isSolution függvényt, amely paraméterként vár négy számot (pl. legyenek a, b, c, y), és eldönti, hogy az első három szám (mint együtthatók) által reprezentált másodfokú egyenletnek megoldása-e a negyedik (tehát y megoldása-e a * x ^ 2 + b * x + c egyenletnek)!

--(Extra feladat): megoldások keresésére megírhatsz egy solve függvényt, amely az egyenlet együtthatóit várja paraméterül, és a megoldóképlet alapján kiszámolja a lehetséges megoldás(oka)t (elég akár csak az egyiket)!

--Definiáld az isSolution' függvényt, amely ugyanúgy működik, mint az isSolution, de csak Integer típusú paraméterekre működik! Megjegyzés: Tesztelő most nem teszteli, hogy ez a függvény tényleg nem működik pl. Double paraméterekkel.

--(Nehezebb extra feladat): Definiáld az órai mult függvényt, amely párokkal reprezentált tört számokat szoroz össze, de most az eredmény legyen leegyszerűsítve.

isSolution :: Float  -> Float  -> Float  -> Float  -> Bool 
isSolution a b c y = x1 == y || x2 == y
                where
                    x1 = e + sqrt d / (2 * a)
                    x2 = e - sqrt d / (2 * a)
                    d = b * b - 4 * a * c
                    e = - b / (2 * a)

solve::Float -> Float -> Float -> (Float, Float )
solve a b c  = (x1,x2)
                where
                    x1 = e + sqrt d / (2 * a)
                    x2 = e - sqrt d / (2 * a)
                    d = b * b - 4 * a * c
                    e = - b / (2 * a)

isSolution':: Integer   -> Integer  -> Integer  -> Integer   -> Bool 
isSolution' a b c y = a*y^2 + b*y + c == 0

--vagy
isSolution'':: Integer   -> Integer  -> Integer  -> Integer   -> Bool 
isSolution'' a b c y = x1 == fromIntegral y || x2 == fromIntegral y
                where
                    x1 = (fromIntegral (-b) + e) / fromIntegral (2 * a)
                    x2 = (fromIntegral (-b) - e) / fromIntegral (2 * a)
                    d :: Integer
                    d = b * b - 4 * a * c
                    e = sqrt (fromInteger d)



