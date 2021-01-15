
import Data.Char

-- Hasznos beépített függvények:
-- kerekítések (ceiling, floor, round), truncate, exp, chr, ord, digitToInt, fromIntegral, pi, odd, even, fst, snd

calc :: Integer -> Char -> Integer -> Double
calc a '+' b = fromIntegral (a + b)
calc a '-' b = fromIntegral (a - b)
calc a '*' b = fromIntegral (a * b)
calc a '/' b = fromIntegral a / fromIntegral b

-- if, case, pattern mathcing, guard (is0)
-- pattern matching

is0 :: Int -> Bool
--is0 a = a == 0
is0 a = if a == 0 then True else False

is0' :: Int -> Bool
is0' 0 = True
is0' _ = False

is0'' :: Int -> Bool
is0'' a = case a of
              0 -> True
              _ -> False

is0''' :: Int -> Bool
is0''' a
  | a == 0    = True
  | otherwise = False

-- Integerek listájánal fejeleme (error függvény)
-- lista: [], (x:xs)
hd :: [Integer] -> Integer
hd []     = error "Empty list!"
hd (x:xs) = x

-- Integerek listájának "farka"/vége
tl :: [Integer] -> [Integer]
tl []     = error "Empty list!"
tl (x:xs) = xs

-- Üres-e a lista?
isEmpty :: [Integer] -> Bool
isEmpty [] = True
isEmpty _  = False
-- isEmpty l = l == []

-- Alakítsuk át egy string első elemét lower/upper case karakterre


-- Szám-e a paraméter


-- Betű-e a paraméter
