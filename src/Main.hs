module Main where

import System.Exit
import Test.HUnit
import Latex2MathMLConverter.Scanner.Main
import Latex2MathMLConverter.Scanner.Tests
import Latex2MathMLConverter.Parser.Main

main :: IO a
main = do
--    runTestTT tests
    putStrLn $ show  $ parse $ fst $ scan "2+    2 \\frac{1}{2} \\begin{matrix} 2 & \\alpha \\\\ 3 & 4 \\end{matrix} + 2"
--     putStrLn (show (scan "2+    2 \\frac{1}{2} \\begin{matrix} 2 & \\alpha \\\\ 3 & 4 \\end{matrix} + 2"))
    exitSuccess

