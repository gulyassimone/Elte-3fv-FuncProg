--data Bool = False | True
--data IntList = Nil | Cons Int IntList
--data List a = Nil | Cons a (List a)


--data Point = Point Int Int deriving (Show)
--incX :: Point -> Point
--incX (Point x y) = Point (x+1) y


data Point = Point { x :: Int, y :: Int } deriving (Show)
incX :: Point -> Point
-- incX (Point x0 y0) = Point { x = x0 + 1, y = y0 }
incX p@(Point { x = x0 }) = p { x = x0 + 1 }




data IntList = Nil | Cons Int IntList

--showIntList :: IntList -> String
--showIntList Nil = "[]"
--showIntList (Cons x xs) = "[" ++ show x ++ "|" ++ showIntList xs ++ "]"

--instance Show IntList where
-- show = showIntList

instance Show IntList where
    show Nil = "[]"
    show (Cons x xs) = "[" ++ show x ++ "|" ++ show xs ++ "]"


--showIntList :: IntList -> String 
--showIntList Nil = "[]"
--showIntList (Const x xs) = "[" ++ show x ++ "|" ++ showIntList xs ++ "]"

--data IntTree = IEmpty | INode IntTree Int IntTree deriving (Show)

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