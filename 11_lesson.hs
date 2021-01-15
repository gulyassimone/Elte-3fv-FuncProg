import Data.Char
import Data.List

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' pr[]     = [] 
takeWhile' pr (x:xs) 
    | pr x      = x : takeWhile' pr xs
    | otherwise = []


dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' pr[]     = [] 
dropWhile' pr l@(x:xs)
    | pr x      = dropWhile' pr xs
    | otherwise = l

on :: (a -> a -> c) -> (b -> a) -> (b -> b -> c)
on f transf = \ x y -> f (transf x) (transf y)

-- Algebrai adattípusok: felsorolási típusok
-- MyBool <- ketelemu tipus
data MyBool = Mytrue | MyFalse deriving Show

(&&&) :: MyBool -> MyBool -> Bool
Mytrue &&& Mytrue = True
_ &&& _           = False

-- egyelemu tipus: Top
data Top = TT 

data Day = Mon | Tue | Wen | Thu | Fri | Sat | Sun

--isWeekend
isWeekend :: Day -> Bool
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


time :: Int -> Int -> Time
time a b
    | a> 23 || b < 0 = error ("Invalid hour:" ++ show a)
    | b > 59 || b < 0 = error ("Invalid minutes:" ++ show b)
    | otherwise = T a b


--data Maybe a = Just a | Nothing

time' :: Int -> Int -> Maybe Time
time' a b
    | a> 23 || b < 0 = Nothing
    | b > 59 || b < 0 = Nothing
    | otherwise = Just(T a b)



safeDiv :: Int -> Int -> Maybe Int
safeDiv n d 
    | d == 0 = Nothing
    | otherwise = Just $ n `div` d
 
data Answer = Yes | No | DontKnow deriving (Show, Eq)

andAnswer :: Answer -> Answer -> Answer  
andAnswer Yes Yes = Yes
andAnswer Yes DontKnow = DontKnow
andAnswer DontKnow DontKnow = DontKnow
andAnswer No Yes = No
andAnswer No DontKnow = No
andAnswer No No = No
andAnswer DontKnow Yes = DontKnow
andAnswer _ _ = No

