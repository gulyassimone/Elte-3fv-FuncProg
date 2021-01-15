import Data.Char

--Add meg a count függvényt, amely megszámolja, hogy egy feltétel hányszor teljesül egy lista elemeire!
count :: (a -> Bool) -> [a] -> Int
count fv [] = 0
count fv (head:tail) = if fv head 
                  then 1 + count fv tail  
                  else count fv tail

--Add meg a transformFirst függvényt, mely egy String első elemét átalakítja: ha nagybetű, legyen kisbetű, ha kisbetű, legyen nagybetű, ha pedig egyik sem, dobjuk el! Vigyázz, kezeld az üres String esetét a Maybe típus használatával!
transformFirst :: String -> Maybe String
transformFirst [] = Nothing
transformFirst (head:tail)
                | isLower head = Just (toUpper head : tail)
                | isUpper head = Just (toLower head : tail)
                | otherwise = Just tail

--Add meg a negativeFilter függvényt, amely egy feltétel alapján szűri a lista elemeit: az eredmény listában az eredeti lista azon elemei maradjanak, amelyekre a feltétel nem teljesült! Ne használj filter-t ehhez a feladathoz!
negativeFilter :: (a -> Bool) -> [a] -> [a]
negativeFilter cond [] = []
negativeFilter cond (x:xs) = let res = negativeFilter cond xs in
                         if cond x
                         then res
                         else x : res

--Add meg a first függvényt, amely visszadja az első elemét egy listának, amelyre teljesül a paraméterként megkapott predikátum, ha van ilyen elem! Ha nincs ilyen elem, akkor Nothing legyen az eredmény (használj Maybe típust)!
first :: (a -> Bool) -> [a] -> Maybe a
first pr []     = Nothing
first pr (head:tail) = if pr head then Just head else first pr tail

--Add meg a szerszámok típusát (Tool)! Elég modellezni a következő eszközöket: kalapács (Hammer), csavarkulcs (Wrench) és csavarhúzó (Screwdriver). A kalapácsnak egy egész szám segítségével tároljuk a súlyát, míg a csavarkulcsnak egy egész szám mondja meg a méretét (a csavarhúzónak nincsenek paraméterei)! A Tool valósítsa meg az Eq típusosztályt (deriving Eq)!
data Tool = Hammer Int | Wrench Int | Screwdriver deriving (Eq, Show)
--Add meg a createTool függvényt, amely az eszköz szöveges reprezentációja (pl. Hammer esetén "Hammer") és egy szám (a kalapács súlya, vagy a csavarkulcs mérete) segítségével megad egy eszközt! Kezeld az invalid inputot (nem lézező eszköz létrehozása, nem pozitív méret/súly)!\
createTool :: String -> Int -> Maybe Tool
createTool [] x = Nothing
createTool tool w = if w>0 then
            case tool of 
            "Hammer" -> Just(Hammer w)
            "Wrench" -> Just(Wrench w)
            "Screwdriver" -> Just(Screwdriver)
            _->Nothing
        else Nothing
