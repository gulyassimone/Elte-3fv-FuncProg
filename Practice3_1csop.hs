
-- keressük az a * x ^ 2 + b * x + c függvény zérushelyeit

helper :: Double -> Double -> Double -> Double -> Double
helper a b c d = ((-b) + d * sqrt(b ^ 2 - 4 * a * c)) / (2 * a)

solve :: Double -> Double -> Double -> ( Double , Double )
solve a b c = ( helper a b c 1, helper a b c (-1) )

mult :: (Integer , Integer) -> (Integer , Integer) -> (Integer , Integer) 
-- mult (a1, b1) (a2, b2) = (a1 * a2 , b1 * b2)
mult a b = ( fst a * fst b, snd a * snd b )

-- függvények definiálása több egyenlettel
-- i.e. esetszétválasztással megadott függvények

and' :: Bool -> Bool -> Bool
-- and' a b = a && b
and' True  True  = True
and' False True  = False
and' True  False = False
and' False False = False

and'' :: Bool -> Bool -> Bool
and'' True True = True
and'' False _   = False
and'' _ False   = False

and''' :: Bool -> Bool -> Bool
and''' True True = True
and''' _ _       = False

and'''' :: Bool -> Bool -> Bool
and'''' True True = True
and'''' a b       = False

-- badand :: Bool -> Bool -> Bool
-- badand _ _       = False
-- badand True True = True


calc :: Integer -> Char -> Integer -> Integer
calc a '+' b = a + b
calc a '-' b = a - b
calc a '*' b = a * b

