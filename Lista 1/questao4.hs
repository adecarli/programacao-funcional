
data Tree a = Nil | Node a (Tree a) (Tree a) deriving Show

newtype Set a = Set (Tree a)

-- Construction
empty :: Tree a
empty = Leaf

singleton :: a -> Tree a
singleton x = Node x Leaf Leaf

fromList :: Ord a => [a] -> Tree a
fromList xs = undefined

-- Insertion
insert :: Ord a => a -> Tree a -> Tree a
insert n Leaf = singleton n
insert n (Node x t1 t2) 
    | n == x = Node n t1 t2
    | n < x = Node x (insert n t1) t2
    | otherwise = Node x t1 (insert n t2)

-- Deletion
delete :: Ord a => a -> Tree a -> Tree a
delete n Leaf = undefined
delete n (Node x t1 t2)
    | n == x = 