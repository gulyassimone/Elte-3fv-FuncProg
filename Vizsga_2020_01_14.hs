import Data.List
import Data.Char

--Készíts függvényt, amely egy kapott lista 1., 3., és 5. elemét egy hármasba rendezve visszaadja. Feltételezheted, hogy van a listának legalább 5 eleme.

tripletFromList :: [a] -> (a, a, a)
tripletFromList (a:_:b:_:c:x) = (a,b,c) 

--A downAndUpList függvény egész számokat tartalmaz, az első felében n-től -n-ig hetesével csökkenve, majd utána -n + 1-től n-ig ötösével növekedve. Tipp: Használhatsz intervallum kifejezést!
downAndUpList :: Int -> [Int]
downAndUpList l = [l, l-7..(-l)] ++ [(-l)+1,(-l)+6 ..l]

--Készíts függvényt, amely a paraméterként kapott szövegben megszámolja, hogy a szöveg hány karaktere az angol ábécé betűje! Tipp: Használhatsz intervallum kifejezést!

countLetters :: String -> Int
countLetters l = (length . filter (\x ->  (x>= 'A' &&  x <= 'Z') ||((x>= 'a' &&  x <= 'z')))) l

--A calculatePoints függvény számolja meg, hogy egy hallgató hány pontot ért el egy teszten! A paraméterként kapott listában rendezett hármasok találhatóak, az első elem a tippelt válasz, a második a helyes válasz, a harmadik a helyes válaszért járó pontszám. Az eredmény az eltalált válaszokért járó pontszámok össszege legyen!
--calculatePoints :: [(Char, Char, Int)] -> Int
--calculatePoints [] = 0
trd :: (a, b, c) -> c
trd ( _ , _ , a ) = a

fst' :: (a, b, c) -> a
fst' ( a , _ , _ ) = a

snd' :: (a, b, c) -> b
snd' ( _ , a , _ ) = a

calculatePoints :: [(Char, Char, Int)] -> Int
calculatePoints [] = 0
calculatePoints l= foldl (+) 0 (map trd (filter (\x -> fst' x == snd' x) l))

--Készíts függvényt, amely egy számokat tartalmazó lista elejére a minimum, a végére pedig a maximum elemét illeszti! Feltételezheted, hogy a listának legalább 1 eleme van.
minmaxList :: [Int] -> [Int]
minmaxList [] = []
minmaxList l =  ((minimum l) : l) ++ (maximum l) : []

--Készíts függvényt, amely egy számokat tartalmazó listában lévő elemeket párosával összeadja, és az eredményekből új listát készít! Ha páratlan számú elem van a listában, akkor az utolsó elemet illeszd csak az új lista végére változtatás nélkül!
pairSum :: [Int] -> [Int]
pairSum [] = []


--Készíts függvényt, amely összead két Maybe Int típusú adatot! Amennyiben valamelyik input Nothing, az eredmény is Nothing.
addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe Nothing _ = Nothing 
addMaybe _ Nothing = Nothing 
addMaybe a b = Just ((transform a) + (transform b))

transform :: Maybe Int -> Int
transform (Just a) = a

--Készíts függvényt, amely összeadja egy listában tárolt Maybe Int típusú adatokat! Amennyiben valamelyik listabeli elem Nothing, az eredmény is Nothing!
addMaybeList :: [Maybe Int] -> Maybe Int
addMaybeList l 
    | any (==Nothing) l  = Nothing 
    | otherwise          = Just (foldl (+) 0 (map transform l))

--Készíts függvényt, amely egy számokat tartalmazó listában a páros számokat megduplázza, majd a 10-nél nagyobbakat kihagyja az eredményből!
evenOnly :: [Int] -> [Int]
evenOnly [] = []
evenOnly (x:xs) 
    | x > 10 || (x*2) > 10 = evenOnly (xs)
    | even x = (x*2) : [] ++ evenOnly (xs)
    | otherwise =  x: evenOnly (xs)

--Készíts függvényt ami a Collatz sejtés algoritmusát valósítja meg! A bemenet egy pozitív egész szám, amelyből pozitív egészek listáját készítsd el! Ha a szám páros (legyen ez n), akkor a lista következő eleme n fele, egyébként 3 * n + 1. Ha n == 1 akkor leáll az algoritmus. A megadott számmal kezdődjön a lista.

--Érdekesség: A Collatz sejtés szerint a fenti algoritmust használva mindig eljutunk 1-hez, de ezt még nem sikerült bebizonyítani.
collatz :: Int -> [Int]
collatz 1 = [1]
collatz n
    | even n    =   n : collatz (div n 2)
    | otherwise = n : collatz (3 * n + 1)

--A Clock adattípus egy ketyegő óra állapotait valósítja meg. Írj függvényt amely ezt a típust felhasználva visszaszámlál egy megadott értéktől egyesével, Tick tárolja a páros másodperceket, Tock a páratlanokat, amikor nullához érünk a jelzőhangot (Boom) használd! A visszaszámlálás teljes folyamatát add vissza egy listába rendezve!
data Clock = Tick Int | Tock Int | Boom deriving (Eq, Show)

countDown :: Int -> [Clock]
countDown 0 = [Boom]
countDown a 
    | even a = Tick a : countDown (a-1)
    | otherwise = Tock a : countDown(a-1)

--Készíts függvényt mely egy számokat tartalmazó listában szereplő értékek átlagát adja meg! Üres lista esetét Nothing-val kezeljük!
avg :: [Int] -> Maybe Double 
avg [] = Nothing
avg l =  Just ((fromIntegral (foldl (+) 0 l)) / (fromIntegral (length l)))

--A Storage adattípus tárolóeszközöket valósít meg. A Crate egy láda 3 oldalának a hosszát tárolja. A Barrel egy henger magasságát és alapjának sugarát (ilyen sorrendben) tárolja. A Container Storage elemek listáját tárolja.

--Készítsd el a volume függvényt, amely megadja, hogy egy Storage típusú adat mennyi tárolóhellyel (térfogattal) rendelkezik! Container esetén értelemszerűen a tárolt elemek összessége adja a térfogatot. Tipp: téglatest térfogata az oldalak szorzata, henger térfogata az alapjának területe és a magasságának szorzata.
data Storage = Crate Double Double Double | Barrel Double Double | Container [Storage]

volume :: Storage -> Double
volume (Crate a b c) = a * b * c
volume (Barrel a b)  = a * b * b * pi 
volume (Container a) = foldl (+) 0 (map volume a)

--Alább definiáltuk a Tree típust. Készíts függvényt amely Either a b típusú elemeket tartalmazó fákat konvertál Either b a típusúvá!
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Eq, Show)

--flipEitherTree :: Tree (Either a b) -> Tree (Either b a)

--Készíts függvényt, amely email címek részeit rendezi párokba! Az első részbe a '@' karakter előtti azonosító kerüljön, a második részbe a '@' utáni szövegrész legyen egy listába rendezve a '.' karakterek mentén elválasztva! Ha nem szerepel '@' a szövegben, a pár első eleme az egész szöveg és második eleme az üres lista legyen! Ha a '@' karakter után nincs '.', akkor az eredmény pár második eleme egyelemű lista!

--separateEmail :: String -> (String, [String])
separateEmail l = "(" ++ head (splitOn '@' l) ++ ",[" ++ (foldl (++) " " (splitOn '.' (last (splitOn '@' l)))) ++"]"
-- ",[" ++ (last (splitOn '@' l))
--(foldl (**) " " (splitOn '.' (last (splitOn '@' l))))

splitOn  :: Char -> String -> [String]
splitOn  _ "" = []
splitOn  delimiter list =
  map (takeWhile (/= delimiter) . tail)
    (filter (isPrefixOf [delimiter])
       (tails
           (delimiter : list)))