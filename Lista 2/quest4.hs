module Expr where

data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving Show

instance Functor Expr where
    -- fmap :: (a -> b) -> Expr a -> Expr b
    fmap g (Var a) = Var (g a)
    fmap _ (Val a) = Val a
    fmap g (Add exp1 exp2) = Add (fmap g exp1) (fmap g exp2)

instance Applicative Expr where
    -- pure :: a -> Expr a
    pure x = Var x

    -- (<*>) :: Expr (a -> b) -> Expr a -> Expr b
    -- ???

instance Monad Expr where
    -- return :: a -> Expr a
    return x = Var x
    -- (>>=) :: Expr a -> (a -> Expr b) -> Expr b
    (Var x) >>= f = f x
    (Val x) >>= _ = Val x
    (Add exp1 exp2) >>= f = Add (exp1 >>= f) (exp2 >>= f)