import Data.Char
import Data.List

{-
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f d []     = d
foldr' f d (x:xs) = f x (foldr' f d xs)
-}

orList :: [Bool] -> Bool
orList = foldr (||) False

anyList' :: (a -> Bool) -> [a] -> Bool
-- anyList' p = foldr ((||) . p) False
anyList' p = foldr (\ x recxs -> p x || recxs) False

-- Függvénykompozíció: .
-- (f o g) x = f (g x)
{-
composition :: (b -> c) -> (a -> b) -> a -> c
composition f g x = f (g x)
-}

-- orList :: [Bool] -> Bool
-- f : (b -> c) ==> b == [Bool], c == Bool
-- g : (a -> b)
-- map : (a -> Bool) -> [a] -> [Bool] ==> error
-- map p : [a] -> [Bool] ==> a == a
-- map p l : [Bool] ==> error
anyList :: (a -> Bool) -> [a] -> Bool
anyList p = orList . map p

anyList'' :: (a -> Bool) -> [a] -> Bool
anyList'' p = foldr ((||) . p) False


-- Definiálj egy `hasLongLines` nevű függvényt, mely megvizsgálja,hogy egy fájl sorai között van-e legalább 3 szóból álló! Tipp: itt jól jöhet a `lines` és `words` standard könyvtárbeli függvények.

hasLongLines :: String -> Bool
-- hasLongLines = filter (>2) . length
-- hasLongLines s = not (null
                 -- (filter (>2) 
                 -- (map (length . words) 
                 -- (lines s))))
hasLongLines = not . null .
               filter (>2) .
               map (length . words) .
               lines

-- Add meg egy listában található 3-mal osztható páros számok négyzetösszegét! A függvény neve legyen `powSum`!



-- További magasabbrendű függvények:
-- takeWhile/dropWhile



-- on
-- Paraméter: 2 függvény: 1 művelet és egy transzformátor
--                        -> adjuk meg azt a függvényt, amely a műveletet alkalmazza a transzformált értékekre
-- pl.: ((==) `on` isDigit) '1' '2' --> True
--      ((==) `on` isDigit) '1' 'c' --> False
--      ((==) `on` isDigit) 'x' 'c' --> True


-- compare --> LT, EQ, GT
-- maximumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a
-- Add meg egy listában lévő listák közül a leghosszabbat!


