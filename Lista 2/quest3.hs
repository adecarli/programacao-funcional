module QCGraphTest where

import Set
import Relation
import Test.QuickCheck

-- Set generator
instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
    arbitrary = makeSet <$> arbitrary

-- DFS and BFS visit all the same nodes
prop_SearchEquality :: (Ord a, Eq a) =>  Relation a -> a -> Bool
prop_SearchEquality rel val = (breadthFirst rel val) == (depthFirst rel val)
