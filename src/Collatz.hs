module Collatz (runCollatz) where

    import Control.Monad
    import Control.Monad.State
    import Control.Monad.Writer
    import Control.Monad.Identity

    type Collatz = StateT Int (WriterT [Int] Identity)

    tickTell :: Int -> Collatz ()
    tickTell m  = do lift $ tell [m]
                     n <- get
                     put (n+1)

    step :: Int -> Collatz Int
    step n = return $ if even n
        then n `div` 2
        else 3*n + 1

    collatz :: Int -> Collatz ()
    collatz n = do tickTell n
                   unless (n == 1) (collatz <=< step $ n)

    runCollatz :: Int -> (Int, [Int])
    runCollatz n = runIdentity . runWriterT . execStateT (collatz n) $ 0

