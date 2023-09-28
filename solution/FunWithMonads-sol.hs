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

instance Functor (Env r1 r2) where
    fmap f (Env fun) = Env $ \r1 r2 -> f (fun r1 r2)

instance Applicative (Env r1 r2) where
    pure a = Env $ \_ _ -> a
    (Env fab) <*> (Env fa) = Env $ \r1 r2 -> fab r1 r2 $ fa r1 r2

instance Monad (Env r1 r2) where
    (Env ma) >>= amb = Env $ \r1 r2 -> let Env b = amb $ ma r1 r2 
                                        in b r1 r2

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

(FORMULA OF PEARSON CORRELATION HERE)

For example, 
    
    runEnv correlation [1, 11, 23, 42, 1, 2, 5, 90, 22, 65] [228, 14, 98, 33, 17, 0, -1, 33.5, 10, 6] == -0.1967 (± 0.001)

--}

proj1 :: (r1 -> a) -> Env r1 r2 a
proj1 f1 = Env $ \r1 _ -> f1 r1

proj2 :: (r2 -> a) -> Env r1 r2 a
proj2 f2 = Env $ \_ r2 -> f2 r2

runEnv :: Env r1 r2 a -> (r1 -> r2 -> a)
runEnv (Env f) = f

correlation :: Env [Float] [Float] Float
correlation = do
    xMean <- proj1 mean
    yMean <- proj2 mean
    xCent <- proj1 $ fmap (\t -> t - xMean)
    yCent <- proj2 $ fmap (\t -> t - yMean)
    let numerator = dot xCent yCent
    let denominator = sqrt $ dot xCent xCent * dot yCent yCent
    pure $ numerator / denominator 

dot :: [Float] -> [Float] -> Float
dot x y = sum $ zipWith (*) x y

mean :: [Float] -> Float
mean x = sum x / fromIntegral (length x)

