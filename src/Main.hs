module Main where

import System.Exit
import Test.HUnit
--import Latex2MathML.Scanner.Tests
import System.Environment
import Control.Monad.Trans.Either(eitherT)
import Latex2MathML.Utils.Functions
import Latex2MathML.Scanner.Main
import Latex2MathML.Parser.Main

main :: IO a
main =  do
--    _ <- runTestTT tests

--    arguments <- getArgs
--    eitherT print (mapM_ readContentAndProcess) (parseArguments arguments)
    eitherT (print) (print) (scan "\\sqrt[3]8 \\sqrt[3]{8} \\frac[]12 \\frac[]{1}{2}" >>= parse)
    exitSuccess