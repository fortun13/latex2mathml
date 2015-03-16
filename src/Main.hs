module Main where

import System.Exit
import Test.HUnit
import Scanner.Main
import Scanner.Tests

main = do
    runTestTT tests
--    putStrLn (show (scan "2+2 \\frac{1}{2} \\begin{matrix} 2 & 2 \\\\ 3 & 4 \\end{matrix} + 2"))
    exitWith ExitSuccess

