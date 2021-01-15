Kő-papír-olló bajnokság
Mielőtt nekikezdenél…
A nagybeadandó egy közepes méretű Haskell program elkészítése az alábbi specifikáció alapján. A megoldáshoz az előadáson és gyakorlati órákon már tárgyalt fogalmakat és nyelvi eszközöket kell alkalmazni, egyetlen kiegészítéssel: a feladat megoldásának pragmatikus megvalósításához felsorolási típust is érdemes használni. A felsorolási típusok Haskellben hasonlóan használhatók, mint más, széles körben használt nyelvekben. A felsorolási típus nevesített értékeket sorol fel, amely értékekre a programban a nevükkel lehet hivatkozni.

Haskellben felsorolási (enum) típust a data kulcsszóval lehet definiálni:

data MyType = EnumValue1 | EnumValue2 | ...
Felsorolási típusra az egyik legegyszerűbb példa a Bool típus:

data Bool = False | True
A | operátor ‘vagy’-ként olvasható, alternatívát jelöl; tehát a Bool típus False vagy True értéket vehet fel. Ezzel a kiegészítéssel már minden tudás birtokában vagytok, ami szükséges a feladat megoldásához és megvalósításához.

Jel típus
A beadandóban egy kő-papír-olló bajnokságot fogunk szimulálni. Elsőként definiáljuk a Sign típust, amely a kő-papír-olló játéknak megfelelő kézjeleket tartalmazza:

data Sign = Rock | Paper | Scissors deriving (Eq, Show)
A deriving (Eq, Show) lehetővé teszi, hogy egyenlőséget vizsgálj (pl. Paper == Rock) a Sign típusértékei között, illetve hogy ezeket meg tudd jeleníteni. Ezzel nem kell foglalkoznod a feladat megoldása során, a kurzus a félév második felében részletesen tárgyalni fogja a típusosztályok megvalósítási módszereit.

Példa a Sign használatára:

A signListConverter függvény egy Sign típusú listából Sign típusú listát készít úgy, hogy Rock-ból Paper-t, Paper-ből Scissors-t, és Scissors-ből Rock-ot készít. Ehhez a convert segédfüggvényt használja fel.

convert :: Sign -> Sign
convert Rock = Paper
convert Paper = Scissors
convert Scissors = Rock

signListConverter :: [Sign] -> [Sign]
signListConverter [] = []
signListConverter (x:xs) = (convert x) : signListConverter xs

-- Teszteset:
signListConverter [Rock, Paper, Scissors] == [Paper, Scissors, Rock]
Feladatok
A feladatok megoldásához használhatjátok a standard könyvtár függvényeit, de nem kötelező. A következő függvények hasznosak lehetnek a megoldás megfogalmazásában: abs, cycle, filter, foldl, foldr, map, repeat, sum, take, zip, zipWith.

A feladatok megoldása során bármennyi segédfüggvényt definiálhattok.

Játékosok
Készítsd el a játékosokat! Minden játékost egy pár reprezentál, melynek első eleme egy String (a játékos neve), a második eleme pedig a stratégiája, amely egy végtelen, Sign típusú lista.

Emlékeztető: a type kulcsszóval típusszinonimát lehet megadni.

type Player = (String, [Sign])
1. játékos: Anna (2 pont)
anna neve “Anna” és stratégiája Rock, Paper, Scissors egymás után ismételve végtelen hosszan.

anna :: Player

-- PÉLDA TESZTEK:
fst anna == "Anna"
head (snd anna) == Rock
take 4 (snd anna) == [Rock,Paper,Scissors,Rock]
2. játékos: John (1 pont)
john neve “John” és stratégiája, hogy mindig Paper-t mutat.

john :: Player

-- PÉLDA TESZTEK:
fst john == "John"
head (snd john) == Paper
take 4 (snd john) == [Paper,Paper,Paper,Paper]
3. játékos: George (3 pont)
George játékost már félig elkészítettük, már csak a stratégiát létrehozó függvényt (georgeStrategy) kell megadnod.

george :: Player
george = ("George", georgeStrategy [1..])
George attól függően választ, hogy hanyadik menetnél tart a mérkőzés:

Először megvizsgálja, hogy a menet sorszáma 7-tel osztva 5 maradékot ad-e, ha igen, akkor Rock a következő választása
Ha az első feltétel nem teljesült, akkor megvizsgálja, hogy a menet sorszáma 3-mal osztva 0 maradékot ad-e, ha igen, akkor Paper a következő választása
Ha az első két feltétel nem teljesült akkor Scissors a következő választása
georgeStrategy :: [Int] -> [Sign]

-- PÉLDA TESZTEK:
head (snd george) == Scissors
take 7 (snd george) == [Scissors,Scissors,Paper,Scissors,Rock,Paper,Scissors]
Eredmény típus
A Result is egy felsorolási típus, amely Player1, Player2 és Draw értéket vehet fel, ezt fogjuk felhasználni egy forduló győztesének meghatározására. Értékek jelentése: - Player1 első játékos nyert - Player2: második játékos nyert - Draw: döntetlen

data Result = Player1 | Player2 | Draw deriving (Eq, Show)
Jelek összehasonlítása (2 pont)
Készítsd el a compareSign függvényt amely két Sign paramétere alapján megadja a nyertes játékost, ha van ilyen! A győztes meghatározásához a kő-papír-olló megszokott szabályait alkalmazzuk: kő legyőzi az ollót, olló legyőzi a papírt, a papír legyőzi a követ, ugyanolyan jelek esetében pedig döntetlen az eredmény.

Ha az első paraméteré a győzelem, akkor az első játékos nyert, az eredmény pedig Player1.
Ha a második paraméteré a győzelem, akkor a második játékos nyert, az eredmény pedig Player2.
Ha döntetlen a kimenetel, akkor a függvény Draw-t ad vissza.
compareSign :: Sign -> Sign -> Result

-- PÉLDA TESZTEK:
compareSign Rock Paper == Player2
compareSign Paper Rock == Player1
compareSign Scissors Scissors == Draw
Jelek listájának összehasonlítása (3 pont)
Készitsd el a compareSigns függvényt a compareSign függvény felhasználásával! A listákban lévő elemeket (a mérkőzések során mutatott jeleket) páronként összehasonlítva készíts egy új listát az eredményekből!

compareSigns :: [Sign] -> [Sign] -> [Result]

-- PÉLDA TESZTEK:
compareSigns [Rock] [Rock] == [Draw]
compareSigns [Paper] [Rock] == [Player1]
compareSigns [Paper, Scissors, Rock] [Rock, Scissors, Paper] == [Player1,Draw,Player2]
Játékosok küzdelme (1 pont)
Készitsd el a fightPlayers függvényt, mely két játékos stratégiáját méretteti meg egymással szemben! Használd fel a compareSigns függvényt!

fightPlayers :: Player -> Player -> [Result]

-- PÉLDA TESZTEK:
head (fightPlayers anna john) == Player2
head (fightPlayers anna george) == Player1
head (fightPlayers john george) == Player2
Pontszámok (1 pont)
Készitsd el az evaluateResult függvényt, amely 1-et ad vissza ha az eredmény Player1, -1-et ha Player2 és 0-át, ha Draw!

evaluateResult :: Result -> Int

-- PÉLDA TESZTEK:
map evaluateResult [Player1, Draw, Player2] == [1,0,-1]
Pontszámok összegzése (3 pont)
Készítsd el a calculatePoints függvényt, amely egy Result típusú listából előállít egy versenypontszámot. Minden Result értéket át kell alakítani az evaluateResult függvény segítségével, majd összeadni az eredményeket.

calculatePoints :: [Result] -> Int

-- PÉLDA TESZTEK:
calculatePoints [Draw] == 0
calculatePoints [Player1, Player2, Draw] == 0
calculatePoints [Player1, Player2, Player2] == (-1)
Játékosok véges küzdelme (1 pont)
Készítsd el a fightPlayersUntil függvényt, amely két játékos stratégiáját méretteti meg egymással szemben, és visszaadja az eredmény első m elemét! Használd fel a fightPlayers függvényt!

fightPlayersUntil :: Player -> Player -> Int -> [Result]

-- PÉLDA TESZTEK:
fightPlayersUntil anna george 0 == []
fightPlayersUntil anna george 10 == [Player1,Player2,Player1,Player1,Player1,Player1,Player1,Player2,Player1,Player1]
Verseny (3 pont)
Készítsd el a tournament függvényt, amely két játékost és egy egész számot vár paraméterül! Két játékost megmérettet egymás ellen, kiszámolja az eredmények alapján a pontszámot (használd: calculatePoints és fightPlayersUntil), majd összegzi azt.

A harmadik paraméter a csaták számát jelzi, ne feledd, hogy eddig végtelen listákkal dolgoztunk! A visszatérési érték pedig egy pár, melynek első eleme a győztes neve, a második a pontszáma.

Amennyiben a pontszám nagyobb mint 0, akkor az első játékos nyert, ha kisebb, akkor a második. Ha 0 a pontszám (döntetlen), akkor a két nevet fűzze össze egy / karakterrel! A pontszám abszolút értékét adja vissza, ne legyenek negatív eredmények!

tournament :: Player -> Player -> Int -> (String, Int)

-- PÉLDA TESZTEK:
tournament anna george 1234 == ("Anna",354)
tournament anna john 10 == ("John", 1)
tournament john george 10 == ("George",5)