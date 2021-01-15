

--Defináld egy helyvektor nyújtását konstansszorosára! Legyen a függvény neve stretch. Tesztek:
stretch :: (Integer , Integer )-> Integer  -> (Integer , Integer )
stretch (a, b) y = (a*y, b*y)

--Számítsd ki két pont távolságát a síkban! Legyen a függvény neve distance. Tesztek:
distance :: (Integer , Integer )-> (Integer , Integer ) -> Float 
distance (a1,a2) (b1,b2) = sqrt (fromIntegral $ (b1-a1)^2 + (b2-a2)^2)

--Definiáld újra a logikai „vagy” operátortmintaillesztéssel! Legyen a függvény neve or'. Tesztek:
or' :: Bool -> Bool -> Bool 
or' False False = False 
or' _ _         = True 

--Definiáld az isSpace függvényt, ami egy bemeneti paraméterről eldönti, hogy space (' ') karakter-e! Tesztek:
isSpace :: Char  -> Bool 
isSpace ' ' = True 
isSpace _   = False 