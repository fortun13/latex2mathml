module Main where

import System.Exit
--import Test.HUnit
import Scanner.Main
--import Scanner.Tests
--import NewScanner.Parser

main = do
--    runTestTT tests
--    print (show (parseCSV "aaa,b,c\n"))
--    putStrLn (show (fst (scan "\\frac{2}%stupid place for comment \n{3}")))
    putStrLn (show (fst (scan "2+2   +    5")))
    exitWith ExitSuccess

