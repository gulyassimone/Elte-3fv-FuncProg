data Sign = Rock | Paper | Scissors deriving (Eq, Show)

type Player = (String, [Sign])
      
anna :: Player 
anna = ("Anna",  cycle [Rock, Paper, Scissors]) 

john :: Player
john = ("John", repeat Paper)

george :: Player
george = ("George", georgeStrategy[1..])

georgeStrategy :: [Int] -> [Sign]
georgeStrategy l = map (\x -> decode x ) l where 
        decode x 
                | mod x 7 == 5 = Rock
                | mod x 3 == 0 = Paper
                | otherwise = Scissors  

data Result = Player1 | Player2 | Draw deriving (Eq, Show)

compareSign  :: Sign -> Sign -> Result
compareSign x y
                | x==y = Draw
                | or [and [(x == Rock),  (y == Paper)], and [(x == Paper),  (y == Scissors)],and [(x == Scissors),  (y == Rock)] ]= Player2 
                | otherwise = Player1

compareSigns  :: [Sign] -> [Sign] -> [Result]
compareSigns  a b = zipWith (\x y -> compareSign x y) a b 

fightPlayers :: Player -> Player -> [Result]
fightPlayers a b= compareSigns (snd a) (snd b)

evaluateResult :: Result -> Int
evaluateResult r
                | r==Player1 = 1
                | r==Player2 = -1
                | otherwise = 0

calculatePoints :: [Result] -> Int
calculatePoints l = foldr (+) 0 (map (\x -> evaluateResult x) l) 

fightPlayersUntil :: Player -> Player -> Int -> [Result]
fightPlayersUntil a b n = take n (fightPlayers a b)

tournament :: Player -> Player -> Int -> (String, Int)
tournament a b n 
                | points > 0 = (fst a, abs points )
                | points < 0 = (fst b, abs points )
                | otherwise = ( (++)  (fst a) ((++) "/" (fst b )) , abs points ) where 
                        points = calculatePoints ( fightPlayersUntil a b n) 