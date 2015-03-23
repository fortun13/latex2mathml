module Main where

import System.Exit
import Test.HUnit
import Latex2MathML.Scanner.Main
import Latex2MathML.Scanner.Tests
import Latex2MathML.Parser.Main

main :: IO a
main = do
    _ <- runTestTT tests
    --print $ scan "\\{ \\["
    --print $ scan "\\forall x \\in X, \\quad \\exists y \\leq \\epsilon"
    --print $ scan "\\int\\limits_a^b"
    --putStrLn $ show  $ parse $ fst $ scan "2+    2 \\frac{1}{2} \\begin{matrix} 2 & \\alpha \\\\ 3 & 4 \\end{matrix} + 2"
    --putStrLn (show (scan "2+    2 \\frac{1}{2} \\begin{matrix} 2 & \\alpha \\\\ A & B \\end{matrix} + 2"))
    exitSuccess

