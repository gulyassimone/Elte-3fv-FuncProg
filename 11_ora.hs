data Tree a = Empty | Node (Tree a) a (Tree a) deriving (Show)
type IntTree = Tree Int --typus szinoníma
-- type String = [Char]

--instance Show (Tree a) where
-- show Empty = "empty"
-- show (Node _ _ _) = "node"

instance Eq (Tree a) where ---elem eq vizsgálatát használja az elem
    Empty == Empty           = True 
    Node _ _ _ == Node _ _ _ = False 
-- Empty == Empty = True
-- Node _ _ _ == Node _ _ _ = False

--mapTree f Empty = Empty
--mapTree f (Node l x r) = Node (mapTree f l) (f x) (mapTree f r)

instance Functor Tree where
    fmap f Empty = Empty
    fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)
--fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

-- sumIntTree :: IntTree -> Int
-- sumIntTree Empty = 0
-- sumIntTree (Node l x r) = sumIntTree l + (x + sumIntTree r)

sumIntTree :: Int -> IntTree -> Int
sumIntTree z Empty = z
sumIntTree z (Node l x r) = (sumIntTree (x + sumIntTree z r) l)

instance Foldable Tree where
    foldr f z Empty = z
    foldr f z (Node l x r) = foldr f (x `f` (foldr f z r)) l


infTree :: a->Tree a
infTree x = Node Empty x (infTree x ) --lehet rá műveleteket alkalmazni


--data T                    void
--Data T()                  unit, megtörtént de nem kell semmit sem visszaadnia
--data T = A | B            elemek data Bool = False | True
--data T a = A a | B        Maybe
data Talan a = Nincs | Van a deriving(Show)
--data T a b = A a | B      Tuple
data Point x y = Point x y
--data T a b = A a | B a    Either
data Egyik x y = Bal x | Jobb y

pairWithBoolToEither :: (Bool,a) -> Either a a
pairWithBoolToEither = undefined -- ?

eitherToPairWithBool :: Either a a -> (Bool,a)
eitherToPairWithBool = undefined -- ?

functionFromBoolToPair :: (Bool -> a) -> (a,a)
functionFromBoolToPair = undefined -- ?

pairToFunctionFromBool :: (a,a) -> (Bool -> a)
pairToFunctionFromBool = undefined -- ?
--succ, pred - előtte vagy utána Enum