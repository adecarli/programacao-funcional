
module MySet ( Set,
    empty,  -- Set a
    sing,   -- a -> Set a
    memSet, -- Ord a => Set a -> a -> Bool
    union,  -- Ord a => Set a -> Set a -> Set a
) where
    
import MyTree 

newtype Set a = Set (Tree a) deriving Show

empty :: Set a
empty = Set Nil

sing :: a -> Set a
sing x = Set (Node x Nil Nil)

memSet :: Ord a => Set a -> a -> Bool
memSet (Set Nil) _ = False
memSet (Set (Node x t1 t2)) y
    | y == x = True
    | y < x  = memSet (Set t1) y
    | otherwise = memSet (Set t2) y

union :: Ord a => Set a -> Set a -> Set a
union (Set t1) (Set t2) = Set (uni t1 t2)

uni :: Ord a => Tree a -> Tree a -> Tree a
uni Nil t2 = t2
uni t1 Nil = t1
uni t1 t2
    | x < y = insTree x (uni newt1 t2)
    | x == y = insTree x (uni newt1 newt2)
    | otherwise = insTree y (uni t1 newt2)
        where
            x = treeVal t1
            y = treeVal t2
            newt1 = delete x t1
            newt2 = delete y t2

inter :: Ord a => Set a -> Set a -> Set a
inter (Set t1) (Set t2) = Set (int t1 t2)

int :: Ord a => Tree a -> Tree a -> Tree a
int _ Nil = Nil
int Nil _ = Nil
int t1 t2
    | x < y = int newt1 t2
    | x == y = insTree x (int newt1 newt2)
    | otherwise = int t1 newt2
        where
            x = treeVal t1
            y = treeVal t2
            newt1 = delete x t1
            newt2 = delete y t2

diff :: Ord a => Set a -> Set a -> Set a
diff (Set t1) (Set t2) = Set (dif t1 t2)

dif :: Ord a => Tree a -> Tree a -> Tree a
dif Nil _ = Nil
dif t1 Nil = t1
dif t1 t2
    | x < y = insTree x (dif newt1 t2)
    | x == y = dif newt1 newt2
    | otherwise = dif t1 newt2
        where
            x = treeVal t1
            y = treeVal t2
            newt1 = delete x t1
            newt2 = delete y t2

subSet :: Ord a => Set a -> Set a -> Bool
subSet (Set t1) (Set t2) = subS t1 t2

subS :: Ord a => Tree a -> Tree a -> Bool
subS Nil _ = True
subS _ Nil = False
subS t1 t2
    | x < y = False
    | x == y = subS newt1 newt2
    | otherwise = subS t1 newt2
        where
            x = treeVal t1
            y = treeVal t2
            newt1 = delete x t1
            newt2 = delete y t2

eqSet :: Eq a => Set a -> Set a -> Bool
eqSet (Set t1) (Set t2) = (t1 == t2)

leqSet :: Ord a => Set a -> Set a -> Bool
leqSet (Set t1) (Set t2) = (t1 <= t2)