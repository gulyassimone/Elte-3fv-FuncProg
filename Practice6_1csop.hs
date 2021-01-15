
import Data.List
import Data.Char

-- compress, decompress?
hoursMinutes = [ (a, b) | a <- [0..23], b <- [0..59]]

compress :: String -> [(Int, Char)]
compress l = [ (length l', head l') | l' <- group l]

decompress l = concat [replicate (fst p) (snd p) | p <- l]

-- fac, fib
fac :: Int -> Int
fac 0 = 1
-- fac 1 = 1
-- fac 2 = 2
-- fac 3 = 6
fac n = n * fac (n - 1)


fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

{-
  fib 3 --> fib (3 - 1) + fib (3 - 2) -->
  --> fib 2 + fib (3 - 2) -->
  --> (fib (2 - 1) + fib (2 - 2)) + fib (3 - 2)
  --> (fib 1 + fib (2 - 2)) + fib (3 - 2)
  --> ( 1 + fib (2 - 2)) + fib (3 - 2)
  --> (1 + fib 0) + fib (3 - 2) 
  --> (1 + 0) + fib (3 - 2)
  --> (1 + 0) + fib 1 --> (1 + 0) + 1 -*-> 2
-}


-- range
range :: Int -> Int -> [Int]
range a b
  | a == b = [a]
  | a <  b = a : range (a + 1) b
  | a >  b = []


-- polinom Horner-sémával: a0 + x * (a1 + x * (a2 + x * (a3 + ... + x * (an-1 + an * x) ... )))
horner :: [Int] -> Int -> Int
horner []     _ = error "No polinom"
horner [a0]   _ = a0
horner (a:xs) y = a + y * horner xs y


-- Példa átírás:
-- horner [1, 1] 4 --> 1 + 4 * horner (1:[]) 4 --> 1 + 4 *(1) --> 5

-- min/max

-- elem

-- zip, unzip

-- isPrefixOf

-- nub <- no duplications

