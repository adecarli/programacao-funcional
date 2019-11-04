module QCTreeTest where

import Tree
import Test.QuickCheck

-- Tree generator
instance (Arbitrary a, Ord a) => Arbitrary (Tree a) where
    arbitrary = tree

tree :: (Arbitrary a, Ord a) => Gen (Tree a)
tree = frequency [(1, return Nil), (4, insTree <$> arbitrary <*> tree)]

-- # of leaves <= # internal nodes + 1
prop_NodeLeafRelation :: Tree a -> Bool
prop_NodeLeafRelation t = leaves <= nodes + 1
    where 
        nodes = countInternalNodes t
        leaves = countLeaves t

-- # of nodes <= 2 ^ (height + 1) - 1
prop_NodeHeightRelation :: Tree a -> Bool
prop_NodeHeightRelation t = nodes <= 2 ^ (height + 1) - 1
    where
        nodes = countNodes t
        height = calculateHeight t

-- every value to the left of the node is smaller than it's value
-- and every value to the right of the node is greater than it's value
prop_SearchTree :: Tree Int -> Bool
prop_SearchTree (Nil) = True
prop_SearchTree (Node _ Nil Nil) = True
prop_SearchTree (Node x t1 t2) = maxValue x t1 && minValue x t2 && prop_SearchTree t1 && prop_SearchTree t2

maxValue :: (Ord a) => a -> Tree a -> Bool
maxValue _ Nil = True
maxValue x (Node y t1 t2) = x >= y && maxValue x t1 && maxValue x t2

minValue :: (Ord a) => a -> Tree a -> Bool
minValue _ Nil = True
minValue x (Node y t1 t2) = x <= y && minValue x t1 && minValue x t2
   
countInternalNodes :: Tree a -> Int
countInternalNodes (Nil) = 0
countInternalNodes (Node _ Nil Nil) = 0 -- this is a leaf
coutnInternalNodes (Node _ t1 t2) = 1 + countInternalNodes t1 + countInternalNodes t2

countLeaves :: Tree a -> Int
countLeaves (Nil) = 0
countLeaves (Node _ Nil Nil) = 1 -- this is a leaf
countLeaves (Node _ t1 t2) = countLeaves t1 + countLeaves t2
        
countNodes :: Tree a -> Int
countNodes (Nil) = 0
countNodes (Node _ t1 t2) = 1 + countNodes t1 + countNodes t2

calculateHeight :: Tree a -> Int
calculateHeight (Nil) = 0
calculateHeight (Node _ Nil Nil) = 0
calculateHeight (Node _ t1 t2) = 1 + max (calculateHeight t1) (calculateHeight t2)