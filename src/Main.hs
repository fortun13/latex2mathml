module Main where

import System.Exit
import Test.HUnit
--import Latex2MathML.Scanner.Tests
import System.Environment
import Control.Monad.Trans.Either(eitherT)
import Latex2MathML.Utils.Functions
--import Latex2MathML.Scanner.Main
--import Latex2MathML.Parser.Main

main :: IO a
main =  do
--    _ <- runTestTT tests
--    eitherT (print) (saveToFile "output.xhtml") (scan "\\frac{\\alpha}{\\beta}" >>= parse >>= generate)
--    eitherT (print) (saveToFile "output2.xhtml") (scan "\\int^{10}_1 \\int_{11}^{20} \\sum^{10}_{1} \\prod_{1}^{10} \\lim_{x \\to \\infty}^{12} \\exp(-x) = 0" >>= parse >>= generate)
--    eitherT (print) (saveToFile "output3.xhtml") (scan "A_{m,n} = \\begin{Vmatrix} \\begin{Bmatrix} 1 & 2 \\\\ 3 & 4 \\end{Bmatrix} & 2 \\\\ 3 & 4 \\end{Vmatrix}" >>= parse >>= generate)
--    eitherT (print) (saveToFile "output4.xhtml") (scan "A_{m,n} = \\begin{matrix} 1 & 2 \\\\ 3 & 4 \\end{matrix} A_{m,n} = \\begin{pmatrix} 1 & 2 \\\\ 3 & 4 \\end{pmatrix} A_{m,n} = \\begin{bmatrix} 1 & 2 \\\\ 3 & 4 \\end{bmatrix} A_{m,n} = \\begin{Bmatrix} 1 & 2 \\\\ 3 & 4 \\end{Bmatrix} A_{m,n} = \\begin{vmatrix} 1 & 2 \\\\ 3 & 4 \\end{vmatrix} A_{m,n} = \\begin{Vmatrix} 1 & 2 \\\\ 3 & 4 \\end{Vmatrix}" >>= parse >>= generate)

    arguments <- getArgs
    eitherT print (mapM_ readContentAndProcess) (parseArguments arguments)
--    eitherT (print) (print) (scan "\\frac1" >>= parse)
    exitSuccess