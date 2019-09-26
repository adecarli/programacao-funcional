import Data.List

data Prop = Const Bool | Var Char | Not Prop | And Prop Prop | Imply Prop Prop deriving Show

type Subst = [(Char, Bool)]

eval :: Subst -> Prop -> Bool
eval _ (Const x) = x
eval [] (Var _) = undefined
eval (x:xs) (Var c) = if (fst x == c) then snd x else eval xs (Var c)
eval xs (Not p) = not (eval xs p)
eval xs (And p1 p2) = (eval xs p1) && (eval xs p2)
eval xs (Imply p1 p2) = not (eval xs p1) || (eval xs p2)

vars :: Prop -> [Char]
vars (Const b) = []
vars (Var c) = [c]
vars (Not p) = vars p
vars (And p1 p2) = vars p1 ++ vars p2
vars (Imply p1 p2) = vars p1 ++ vars p2

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = [ x:y | x <- [True, False], y <- bools (n-1) ]

substs :: Prop -> [Subst]
substs p = [ zip (nub $ vars p) y | y <- myBools p]
    where myBools = bools . length . nub . vars

isTaut :: Prop -> Bool
isTaut p = isTrue [ eval x p | x <- substs p ]

isTrue :: [Bool] -> Bool
isTrue [] = True
isTrue (x:xs) = if x == True then isTrue xs else False

-- Test cases
a = Var 'A'
b = Var 'B'
c = Var 'C'
false = Const False
true = Const True
-- Tautologies
-- excluded middle
t1 = Imply a a
-- reductio ad absurdum
t2 = Imply (And (Imply (Not a) b) (Imply (Not a) (Not b))) a
-- syllogism
t3 = Imply (And (Imply a b) (Imply b c)) (Imply a c)
-- contraposition
t4 = Imply (Imply a b) (Imply (Not b) (Not a))
t5 = Imply (Imply (Not b) (Not a)) (Imply a b)
t6 = And t4 t5
-- Examples
p1 = a -- False
p2 = Imply (And a b) a -- True
p3 = Not false -- True
p4 = false -- False
p5 = Imply (And (Imply a b) (Imply a c)) (Imply b c) -- False
-- false premise
p6 = Imply (And (Imply a b) b) a -- False 
