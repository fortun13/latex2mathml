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
    exitSuccess

saveToFile :: String -> String -> IO ()
saveToFile filename content = do
    writeFile filename content
    print $ "File " ++ filename ++ " generated!"