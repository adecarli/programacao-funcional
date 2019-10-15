module MyTree (
    Tree(Nil, Node),
    nil,           -- Tree a
    isNil,         -- Tree a -> Bool  
    isNode,        -- Tree a -> Bool
    leftSub,       -- Tree a -> Tree a 
    rightSub,      -- Tree a -> Tree a 
    treeVal,       -- Tree a -> a
    insTree,       -- Ord a => a -> Tree a -> Tree a 
    delete,        -- Ord a => a -> Tree a -> Tree a
    minTree,       -- Ord a => Tree a -> Maybe a
    elemT          -- Ord a => a -> Tree a -> Bool
) where

data Tree a = Nil | Node a (Tree a) (Tree a) deriving Show

nil :: Tree a
nil = Nil

isNil :: Tree a -> Bool
isNil Nil = True
isNil _   = False

isNode :: Tree a -> Bool
isNode Nil = False
isNode _   = True

leftSub :: Tree a -> Tree a
leftSub Nil           = error "leftSub"
leftSub (Node _ t1 _) = t1

rightSub :: Tree a -> Tree a
rightSub Nil           = error "rightSub"
rightSub (Node _ _ t2) = t2

treeVal :: Tree a -> a
treeVal Nil           = error "treeVal"
treeVal (Node x _ _ ) = x

insTree :: Ord a => a -> Tree a -> Tree a
insTree v Nil = Node v Nil Nil
insTree v (Node x t1 t2)
    | v == x = Node v t1 t2
    | v < x  = Node x (insTree v t1) t2
    | v > x  = Node x t1 (insTree v t2)

delete :: Ord a => a -> Tree a -> Tree a
delete v (Node x t1 t2)
    | v < x     = Node x (delete v t1) t2
    | v > x     = Node x t1 (delete v t2)
    | isNil t1  = t2
    | isNil t2  = t1
    | otherwise = join t1 t2

minTree :: Ord a => Tree a -> Maybe a
minTree t
    | isNil t   = Nothing
    | isNil t1  = Just v
    | otherwise = minTree t1
        where 
            t1 = leftSub t
            v  = treeVal t

elemT :: Ord a => a -> Tree a -> Bool
elemT _ Nil = False
elemT v (Node x t1 t2)
    | v < x     = elemT v t1
    | v == x    = True
    | otherwise = elemT v t2

join :: Ord a => Tree a -> Tree a -> Tree a
join t1 t2 = Node mini t1 newt
    where 
        (Just mini) = minTree t2
        newt        = delete mini t2
