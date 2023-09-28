module Tut11Ans where

-- QUESTION 1

type State = Int
newtype ST a = S (State -> (a,State))

apply :: ST a -> State -> (a, State)
apply (S st) x = st x

instance Monad ST where
    (>>=)::ST a -> (a -> ST b) -> ST b
    st >>= f = S (\s -> let (x,s') = apply st s in apply (f x) s')

instance Functor ST where
    fmap :: (a -> b) -> ST a -> ST b
    fmap g st = do
        a <- st
        return $ g a

instance Applicative ST where
    pure :: a -> ST a
    pure x = S (\s -> (x,s))
    
    (<*>) :: ST (a -> b) -> ST a -> ST b
    stf <*> stx = do
        f <- stf
        fmap f stx

-- QUESTION 2

join :: Monad m => m (m a) -> m a
join mma = mma >>= id

-- QUESTION 3

data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving Show

instance Functor Expr where
    fmap :: (a -> b) -> Expr a -> Expr b
    fmap f (Val i) = Val i
    fmap f (Var a) = Var $ f a
    fmap f (Add a1 a2) = Add (fmap f a1) (fmap f a2)

instance Applicative Expr where
    pure :: a -> Expr a
    pure = Var

    (<*>) :: Expr (a -> b) -> Expr a -> Expr b
    (Val i) <*> expra = Val i
    (Var fab) <*> expra = fmap fab expra
    (Add f g) <*> expra = Add (f <*> expra) (g <*> expra)

instance Monad Expr where
    (>>=) :: Expr a -> (a -> Expr b) -> Expr b
    (Val x) >>= f = Val x
    (Var a) >>= f = f a
    (Add x y) >>= f = Add (x >>= f) (y >>= f)

replace :: Eq a => [(a, b)] -> Expr a -> Expr (Maybe b)
replace m ea = do
    a <- ea
    return $ lookup a m