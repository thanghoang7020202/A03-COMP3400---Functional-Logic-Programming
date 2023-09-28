module FunWithMonads 
  ( Env (..)
  , proj1
  , proj2
  , runEnv
  , correlation
  ) where


-- This task is worth 20 POINTS
-- Do NOT modify anything above this line.
-- Do NOT use any imports.


-- You are given the type:

data Env r1 r2 a = Env (r1 -> r2 -> a)

{--

Part 1.

Implement the following instances:

--}

--e is the function take r1 and r2 as arguments and return a
instance Functor (Env r1 r2) where
    -- fmap :: (a -> b) -> Env r1 r2 a -> Env r1 r2 b
    fmap f (Env e) = Env (\x y -> f (e x y))   

instance Applicative (Env r1 r2) where
    -- pure :: a -> Env r1 r2 a
    pure a = Env (\_ _ -> a) 
    -- (<*>) :: Env r1 r2 (a -> b) -> Env r1 r2 a -> Env r1 r2 b
    (<*>) (Env ef) (Env ea) = Env (\x y -> (ef x y) (ea x y))
instance Monad (Env r1 r2) where
    -- (>>=) :: Env r1 r2 a -> (a -> Env r1 r2 b) -> Env r1 r2 b
    (>>=) (Env ea) f = Env (\r1 r2 -> let Env e = f (ea r1 r2) in e r1 r2)

{--
Part 2.

Env emulates two global environments that can not be modified but can be accessed at any time. 

Implement proj1 :: (r1 -> a) -> Env r1 r2 a which will return a projection of its first environment.
Implement proj2 :: (r2 -> a) -> Env r1 r2 a which will return a projection of its second environment.
Implement runEnv :: Env r1 r2 a -> (r1 -> r2 -> a) which will transform the computation inside Env into a Haskell function.

Using do-notation and, possibly, the functions defined above, implement
correlation :: Env [Float] [Float] Float

which will calculate Pearson correlation between two lists.
The lists are guaranteed to be of the same length greater than one with distinct elements, i.e. the correlation will never be undefined. 
Due to floating point imprecision, your answer will be accepted if it is within 0.001 of the correct answer.

                   Σ((x_i - x̄) * (y_i - ȳ))
   pearson =  -------------------------------
              √ (Σ(x_i - x̄)^2 * Σ(y_i - ȳ)^2)

For example, 
    
    runEnv correlation [1, 11, 23, 42, 1, 2, 5, 90, 22, 65] [228, 14, 98, 33, 17, 0, -1, 33.5, 10, 6] == -0.1967 (± 0.001)

--}

proj1 :: (r1 -> a) -> Env r1 r2 a
proj1 fr1 = Env (\r1 r2 -> fr1 r1) 

proj2 :: (r2 -> a) -> Env r1 r2 a
proj2 fr2 = Env (\r1 r2 -> fr2 r2)  

runEnv :: Env r1 r2 a -> (r1 -> r2 -> a)
runEnv (Env e) = e

correlation :: Env [Float] [Float] Float
correlation = do 
    l1 <- proj1 (\x -> x)
    l2 <- proj2 (\y -> y)
    let xbar = (sum l1) / (fromIntegral (length l1))
        ybar = (sum l2) / (fromIntegral (length l2))
        diffxy = [(x - xbar) * (y - ybar) | (x,y) <- zip l1 l2] -- make it as pairs according to the formula
        numer = sum diffxy
        sumx2 = sum [x**2 | ele <- l1, let x = (ele - xbar)]
        sumy2 = sum [y**2 | ele <- l2, let y = (ele - ybar)] 
        result = numer / (sqrt (sumx2 * sumy2))
    return result
