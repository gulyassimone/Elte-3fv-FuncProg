data MyList a = Cons a (MyList a) | Nil deriving (Eq, Show)
data Tree1 a = Node1 (Tree1 a) (Tree1 a) | Leaf1 a deriving (Eq, Show)
data Tree2 a = Node2 (Tree2 a) a (Tree2 a) | Leaf2 a deriving (Eq, Show)

instance Foldable MyList where
    foldr f z Nil = z
    foldr f z (Cons x xs) = f x (foldr f z xs)
instance Functor MyList where
    fmap f Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)
instance Foldable Tree2 where
    foldr f z (Leaf2 a)= f a z
    foldr f z (Node2 l x r) = foldr f (x `f` (foldr f z r)) l
instance Foldable Tree1 where
    foldr f z (Leaf1 a)= f a z
    foldr f z (Node1 l r) = foldr f (foldr f z r) l
instance Functor Tree1 where
    fmap f (Leaf1 a) = Leaf1 (f a)
    fmap f (Node1 l r) = Node1 (fmap f l) (fmap f r)

instance Functor Tree2 where
    fmap f (Leaf2 a) = Leaf2 (f a)
    fmap f (Node2 l x r) = Node2 (fmap f l) (f x) (fmap f r)
    


multTree2 :: (Foldable t, Num b) => t b -> b
multTree2 a = foldl (*) 1 a

multTree1 :: (Foldable t, Num b) => t b -> b
multTree1 a = foldl (*) 1 a

transformMyList :: (Functor f, Num b) => f b -> f b
transformMyList a = fmap (+5) a
transformTree1 :: (Functor f, Num b) => f b -> f b
transformTree1  a = fmap (+5) a
transformTree2 :: (Functor f, Num b) => f b -> f b
transformTree2 a = fmap (+5) a

