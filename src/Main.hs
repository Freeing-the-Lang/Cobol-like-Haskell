module Main where

import CobolSections
import CobolRuntime

main :: IO ()
main = do
    cobolHeader "Cobol-like-Haskell starting..."

    -- Working-storage style variable
    let customerName = "JINHEE"
    let baseNumber   = 7

    perform "INITIALIZATION" $ do
        display "Initializing program..."
        display ("Customer: " ++ customerName)
        display ("Base Number: " ++ show baseNumber)

    perform "COMPUTE-SECTION" $ do
        let result = computeDouble baseNumber
        display ("DOUBLE = " ++ show result)

    perform "TERMINATION" $ do
        display "Program Completed."
