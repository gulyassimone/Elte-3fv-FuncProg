--Definiálj egy inc nevű függvényt, mely paraméterének eggyel megnövelt értékét adja eredményül! Tesztesetek:
inc :: Integer -> Integer
inc n = 1 + n

--(Nehezebb:) Definiálj egy pythagoreanTriple nevű függvényt, mely megmondja három egész számról, hogy azok pitagoraszi számhármasok-e (a három számból kettő négyzetének összege egyenlő a harmadik négyzetével)! Tesztesetek:
pythagoreanTripleAid :: Integer -> Integer ->  Integer ->  Bool 
pythagoreanTripleAid a b c = a^2 + b^2 == c^2 


pythagoreanTriple :: Integer -> Integer ->  Integer ->  Bool 
pythagoreanTriple a b c = pythagoreanTripleAid a b c || pythagoreanTripleAid b c a || pythagoreanTripleAid c a b