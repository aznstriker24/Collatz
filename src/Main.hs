module Main where

    import Collatz

    main :: IO ()
    main = do
        putStrLn "Give me an Integer!"
        n <- fmap read getLine
        putStrLn $ (pretty . runCollatz $ n)

    pretty :: [Int] -> String
    pretty [] = ""
    pretty (n:ns) = show n ++ "\n" ++ pretty ns

