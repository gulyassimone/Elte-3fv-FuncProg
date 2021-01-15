-- Ures tipus: Void
-- 9. gyak --> foldl definíció javítva
-- Javito : Pentek -- kedd

data Time = T Int Int

-- showTime (instance-val szebb lenne)
instance Show Time where
  show (T a b) = show a ++ "." ++ show b

-- smart constructor --> time, csak valid idopontokkal lehessen Time-ot letrehozni
time :: Int -> Int -> Time
time a b
  | a > 23 || a < 0 = error ("Invalid hour: " ++ show a)
  | b > 59 || b < 0 = error ("Invalid minute: " ++ show b)
  | otherwise       = T a b

-- Exception dobasa helyett maradjunk "tisztak"... Milyen tipust lehet hasznalni ehhez?

-- data ErrorTime = Correct Time | Error
-- data Maybe a = Just a | Nothing

time' :: Int -> Int -> Maybe Time
time' a b
  | a > 23 || a < 0 = Nothing
  | b > 59 || b < 0 = Nothing
  | otherwise       = Just (T a b)

-- Mashol is tudjuk hasznalni: pl. biztonsagos div, hd, tl, stb.
safeDiv :: Int -> Int -> Maybe Int
safeDiv n d
  | d == 0    = Nothing
  | otherwise = Just $ n `div` d 

-- safeHd :: [a] -> ???
-- safeHd = undefined

-- Keszitsunk egy map-ot: kulcs-ertek parok listaja. A kulcsok legyenek Intek, ertekek tetszoleges tipusuak.
-- Legyen put, get muvelet (egyszeruseg kedveert csak (==)-t vizsgaljunk)

type Map a = [(Int, a)]

put :: Int -> a -> Map a -> Map a
put k v []            = [(k,v)]
put k v ((k',v'):xs)
  | k == k' = (k, v) : xs
  | k /= k' = (k', v') : put k v xs

get :: Int -> Map a -> Maybe a
get k []            = Nothing
get k ((k', v'):xs)
  | k == k' = Just v'
  | k /= k' = get k xs

-- Hogyan tudnank error uzenetet is tarsitani az elozoekhez? --> Sum tipus

-- data Maybe a = Just a | Nothing String <- hibauzenet

-- data Either a b = Left a | Right b

get' :: Int -> Map a -> Either a String
get' k []            = Right ("Error: key not found:" ++ show k)
get' k ((k', v'):xs)
  | k == k' = Left v'
  | k /= k' = get' k xs


-- 2 vagy 3D-s alakzatot rajzoljunk? (Input : Shape, magassag)

data Shape2D = Circle Double               -- sugar
             | Rectangle Double Double     -- oldalak hossza
  deriving Show
data Shape3D = Cylinder Shape2D Double     -- sugar, magassag
             | Prism Shape2D Double        -- alapok hossza, magassag
  deriving Show

draw :: Shape2D -> Double -> Either Shape2D Shape3D
draw s h
  | h <= 0 = Left s
  | h >  0 = Right ( case s of
                       Circle _ -> Cylinder s h
                       Rectangle _ _ -> Prism s h)

-- Definialjunk lista tipust: Int-eket tartalmazo lista

-- hd, tl, stb...