module Main where

import System.Exit
import Test.HUnit
import Latex2MathML.Scanner.Main
import Latex2MathML.Scanner.Tests
import Latex2MathML.Parser.Main
import Latex2MathML.Generator.Main
import Data.Either

main :: IO a
main = do
--    _ <- runTestTT tests
--    print $ scan "A_{m,n} = \\begin{matrix} a_{1,1} & a_{1,2} & \\cdots & a_{1,n} \\\\ a_{2,1} & a_{2,2} & \\cdots & a_{2,n} \\\\ \\vdots  & \\vdots  & \\ddots & \\vdots  \\\\ a_{m,1} & a_{m,2} \\cdots & a_{m,n} \\end{matrix}" >>= parse
--    print $ scan "\\begin{matrix} asd & fd & gf \\\\ fd & fhd & fhd \\end{matrix}" >>= parse
--    print $ scan "32^{101}_43 ala_{ma}^{kota}" >>= parse
--    print $ scan "\\hat{matrix} \\sqrt{25} \\frac{3}{4}" >>= parse
--    print $ scan "2^long \\sin \\alpha" >>= parse
--    writeFile "output.xhtml" (head $ rights $ [generate $ head $ rights $ [parse $ head $ rights $ [scan "2^long \\sin \\alpha"]]])
    writeFile "output.xhtml" (head $ rights $ [generate $ head $ rights $ [parse $ head $ rights $ [scan "\\hat{matrix} \\sqrt{25} \\frac{3}{4} \\iint"]]])
    print $ head $ rights $ [parse $ head $ rights $ [scan "\\hat{matrix} \\sqrt{25} \\frac{3}{4} \\iint"]]
--    writeFile "output.xhtml" (head $ rights $ [generate $ head $ rights $ [parse $ head $ rights $ [scan "32^{101}_43 ala_{ma}^{kota}"]]])
    -- TODO it's a very primitive way to extract right value from Either
    -- TODO There is module Data.Either.Unwrap (IntelliJ suggest Data.Either.Extra) withs function fromRight (for a single Either, not list)
    -- TODO But compiler don't see it - will find out a solution later

--    print $ scan "\\sqrt{++}" >>= parse
    exitSuccess

