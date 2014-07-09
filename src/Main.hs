module Main where

    import Collatz

    main :: IO ()
    main = do
        putStrLn "Give me an Integer!"
        n <- fmap read getLine
        (m, ms) <- return (runCollatz n)
        putStrLn . pretty $ ms
        putStrLn ("The sequence is " ++ show m ++ " terms long!")

    pretty :: [Int] -> String
    pretty = foldr (\n s-> show n ++ "\n" ++ s ) ""

