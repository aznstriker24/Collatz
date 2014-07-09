module Collatz (runCollatz) where

    import Control.Monad.State
    import Control.Monad.Writer
    import Control.Monad.Identity

    type Collatz = StateT Int (Writer [Int])

    tick :: Collatz ()
    tick = do n <- get
              put (n+1)

    step :: Int -> Collatz Int
    step n | even n    = do tick
                            return (n `div` 2)
           | otherwise = do tick
                            return (3*n + 1)

    collatz :: Int -> Collatz Int
    collatz  n | n == 1 = do lift $ tell [n]
                             return n
               | otherwise = do lift $ tell [n]
                                n' <- step n
                                collatz n'


    runCollatz :: Int -> [Int]
    runCollatz n = execWriter . runStateT (collatz n) $ 0