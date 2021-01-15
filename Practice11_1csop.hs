import Data.Char
import Data.List

-- takeWhile

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' pr []     = []
takeWhile' pr (x:xs)
  | pr x      = x : takeWhile' pr xs
  | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' pr []     = []
dropWhile' pr l@(x:xs)
  | pr x      = dropWhile' pr xs
  | otherwise = l

-- on
-- Paraméter: 2 függvény: 1 művelet és egy transzformátor
--                        -> adjuk meg azt a függvényt, amely a műveletet alkalmazza a transzformált értékekre
-- on :: (a -> a -> c) -> (b -> a) -> (b -> b -> c)
-- pl.: ((==) `on` isDigit) '1' '2' --> True
--      ((==) `on` isDigit) '1' 'c' --> False
--      ((==) `on` isDigit) 'x' 'c' --> True

on :: (a -> a -> c) -> (b -> a) -> (b -> b -> c)
-- on f transf x y = f (transf x) (transf y)
on f transf = \ x y -> f (transf x) (transf y)

-- Algebrai adattípusok: felsorolási típusok
-- MyBool <- ketelemu tipus
data MyBool = MyTrue | MyFalse deriving Show
-- deriving Eq

-- (&&&) :: MyBool -> MyBool -> Bool
-- a &&& b = a == b
(&&&) :: MyBool -> MyBool -> MyBool
MyTrue &&& MyTrue = MyTrue
_      &&& _      = MyFalse


-- egyelemu tipus: Top
data Top = TT

-- Day
data Day = Mon | Tue | Wen | Thu | Fri | Sat | Sun
--  deriving Eq

-- isWeekend
isWeekend :: Day -> Bool
--isWeekend a = a == Sat || a == Sun
isWeekend Sat = True
isWeekend Sun = True
isWeekend _   = False

-- Kicsit bonyolultabb tipusok
-- Time ("ora.perc") (Int, Int)
data Time = T Int Int -- deriving Show

-- showTime (instance-val szebb lenne)
showTime :: Time -> String
showTime (T a b) = show a ++ "." ++ show b

instance Show Time where
  show (T a b) = show a ++ "." ++ show b

-- smart constructor --> time, csak valid idopontokkal lehessen Time-ot letrehozni

-- USTime (AM PM format)

-- USTime to Time

-- Time to USTime