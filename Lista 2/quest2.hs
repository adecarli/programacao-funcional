module QCSetTest where

import Set
import Test.QuickCheck

-- Set generator
instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
    arbitrary = makeSet <$> arbitrary

-- A v B = B v A
prop_CommutativeUnion :: (Eq a, Ord a) => Set a -> Set a -> Bool
prop_CommutativeUnion a b = (union a b) == (union b a)

-- (A v B) v C = A v (B v C)
prop_AssociativeUnion :: (Eq a, Ord a) => Set a -> Set a -> Set a -> Bool
prop_AssociativeUnion a b c = (union (union a b) c) == (union a (union b c))

-- A ^ B = B ^ A
prop_CommutativeIntersection :: (Eq a, Ord a) => Set a -> Set a -> Bool
prop_CommutativeIntersection a b = (inter a b) == (inter b a)

-- A ^ (B ^ C) = (A ^ B) ^ C
prop_AssociativeIntersection :: (Eq a, Ord a) => Set a -> Set a -> Set a -> Bool
prop_AssociativeIntersection a b c = (inter a (inter b c)) == (inter (inter a b) c)

-- A ^ B c A
prop_SubSetIntersection :: (Eq a, Ord a) => Set a -> Set a -> Bool
prop_SubSetIntersection a b = subSet (inter a b) a