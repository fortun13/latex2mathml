module Main where

import System.Exit
import Test.HUnit
import Scanner.Main
import Scanner.Tests

main = do
    runTestTT tests
--    putStrLn (show (fst (scan "[] \\sqrt[n]{1}{2}")))
--    putStrLn (show (fst (scan "\\frac{2}%stupid place for comment \n{3}")))
--    putStrLn (show (fst (scan "2+2   +    5")))
    exitWith ExitSuccess

