module Main where

import System.Exit
import Test.HUnit
import Latex2MathML.Scanner.Main
import Latex2MathML.Scanner.Tests
import Latex2MathML.Parser.Main
import Latex2MathML.Generator.Main
import Data.Either
import Control.Monad.Trans.Either

main :: IO a
main =  do
--    _ <- runTestTT tests
    eitherT (print) (saveToFile "output.xhtml") (hoistEither $ scan "\\frac{\\alpha}{\\beta}" >>= parse >>= generate)
    eitherT (print) (saveToFile "output2.xhtml") (hoistEither $ scan "\\int^{10}_1 \\int_{11}^{20} \\sum^{10}_{1} \\prod_{1}^{10} \\lim_{x \\to \\infty}^{12} \\exp(-x) = 0" >>= parse >>= generate)
    print $ scan "A_{m,n} = \\begin{matrix} \\begin{matrix} 1 & 2 \\\\ 3 & 4 \\end{matrix} & 2 \\\\ 3 & 4 \\end{matrix}" >>= parse
    eitherT (print) (saveToFile "output3.xhtml") (hoistEither $ scan "A_{m,n} = \\begin{matrix} \\begin{matrix} 1 & 2 \\\\ 3 & 4 \\end{matrix} & 2 \\\\ 3 & 4 \\end{matrix}" >>= parse >>= generate)
    exitSuccess

saveToFile :: String -> String -> IO ()
saveToFile filename content = do
    writeFile filename content
    print $ "File " ++ filename ++ " generated!"