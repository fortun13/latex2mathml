module Main where

import System.Exit
import Test.HUnit
import Latex2MathML.Scanner.Main
import Latex2MathML.Scanner.Tests
import Latex2MathML.Parser.Main

main :: IO a
main = do
    _ <- runTestTT tests
--    print $ scan "A_{m,n} = \\begin{matrix} a_{1,1} & a_{1,2} & \\cdots & a_{1,n} \\\\ a_{2,1} & a_{2,2} & \\cdots & a_{2,n} \\\\ \\vdots  & \\vdots  & \\ddots & \\vdots  \\\\ a_{m,1} & a_{m,2} \\cdots & a_{m,n} \\end{matrix}" >>= parse
--    print $ scan "\\frac{1}{2}"
--    print $ scan "2^long" >>= parse
--    print $ scan "\\begin{notMatrix} asd & fd & gf \\\\ fd & fhd & fhd \\end{notMatrix}"
    exitSuccess

