module Main where

import System.Exit
import Test.HUnit
import Latex2MathML.Scanner.Main
--import Latex2MathML.Scanner.Tests
import Latex2MathML.Parser.Main
import Latex2MathML.Generator.Main
import Control.Monad.Trans.Either
import Control.Monad.Error.Class
import Data.Algorithms.KMP

main :: IO a
main =  do
--    _ <- runTestTT tests
--    eitherT (print) (saveToFile "output.xhtml") (scan "\\frac{\\alpha}{\\beta}" >>= parse >>= generate)
--    eitherT (print) (saveToFile "output2.xhtml") (scan "\\int^{10}_1 \\int_{11}^{20} \\sum^{10}_{1} \\prod_{1}^{10} \\lim_{x \\to \\infty}^{12} \\exp(-x) = 0" >>= parse >>= generate)
--    eitherT (print) (saveToFile "output3.xhtml") (scan "A_{m,n} = \\begin{Vmatrix} \\begin{Bmatrix} 1 & 2 \\\\ 3 & 4 \\end{Bmatrix} & 2 \\\\ 3 & 4 \\end{Vmatrix}" >>= parse >>= generate)
--    eitherT (print) (saveToFile "output4.xhtml") (scan "A_{m,n} = \\begin{matrix} 1 & 2 \\\\ 3 & 4 \\end{matrix} A_{m,n} = \\begin{pmatrix} 1 & 2 \\\\ 3 & 4 \\end{pmatrix} A_{m,n} = \\begin{bmatrix} 1 & 2 \\\\ 3 & 4 \\end{bmatrix} A_{m,n} = \\begin{Bmatrix} 1 & 2 \\\\ 3 & 4 \\end{Bmatrix} A_{m,n} = \\begin{vmatrix} 1 & 2 \\\\ 3 & 4 \\end{vmatrix} A_{m,n} = \\begin{Vmatrix} 1 & 2 \\\\ 3 & 4 \\end{Vmatrix}" >>= parse >>= generate)
    content <- readFile "test.tex"
    eitherT (print) (saveToFiles "output") (processContentOfFile content)
    content <- readFile "otherTests.tex"
    eitherT (print) (saveToFiles "secOutput") (processContentOfFile content)
    exitSuccess

processContentOfFile :: String -> EitherT String IO [String]
processContentOfFile content =
    case match (build "$$") content of
        [] -> throwError "No math elements in passed file"
        lst -> splitFileAt content lst

splitFileAt :: String -> [Int] -> EitherT String IO [String]
splitFileAt content lst
    | ((length lst) `mod` 2) == 1 = throwError "Bad input file"
    | otherwise = generateMLFromStrings $ splitFileAt' content lst

splitFileAt' :: String -> [Int] -> [String]
splitFileAt' _ [] = []
splitFileAt' content (x1:x2:xs) = (take (x2-x1-2) (drop (x1+2) content)) : splitFileAt' content xs

generateMLFromStrings :: [String] -> EitherT String IO [String]
generateMLFromStrings = mapM (\x -> scan x >>= parse >>= generate)

saveToFiles :: String -> [String] -> IO ()
saveToFiles rootName lst = saveToFiles' lst rootName 1

saveToFiles' :: [String] -> String -> Int -> IO ()
saveToFiles' [] _ _ = return ()
saveToFiles' (x:xs) fileRoot number = do
    saveToFile (fileRoot ++ (show number) ++ ".xhtml") x
    saveToFiles' xs fileRoot (number+1)

saveToFile :: String -> String -> IO ()
saveToFile filename content = do
    writeFile filename content
    print $ "File " ++ filename ++ " generated!"