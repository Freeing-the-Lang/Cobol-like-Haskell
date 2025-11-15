module CobolRuntime where

display :: String -> IO ()
display msg = putStrLn ("DISPLAY> " ++ msg)

cobolHeader :: String -> IO ()
cobolHeader title = do
    putStrLn "======================================="
    putStrLn ("   COBOL-LIKE-HASKELL PROGRAM")
    putStrLn ("   " ++ title)
    putStrLn "======================================="
