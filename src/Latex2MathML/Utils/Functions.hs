module Latex2MathML.Utils.Functions(parseArguments,processContentOfFile,saveToFiles,readContentAndProcess) where

import Latex2MathML.Generator.Main
import Control.Monad.Trans.Either
import Control.Monad.Error.Class
import Data.Algorithms.KMP
import Latex2MathML.Scanner.Main
import Latex2MathML.Parser.Main

-- ARGUMENTS PROCESSING

parseArguments :: [String] -> EitherT String IO [(String,String)]
parseArguments [] = throwError $ "Empty argument list. " ++ programUsage
parseArguments lst
    | length lst `mod` 2 == 0 = return $ parseArguments' lst
    | otherwise = throwError $ "Bad argument list. " ++ programUsage

parseArguments' :: [String] -> [(String,String)]
parseArguments' [] = []
parseArguments' (filename:rootname:xs) = (filename,rootname) : parseArguments' xs

programUsage :: String
programUsage = "Program usage: ./program firstTexFilePath firstOutputRootFilename [secondTexFilePath ...]"

-- FILES PROCESSING

readContentAndProcess :: (String,String) -> IO ()
readContentAndProcess (filename,rootoutputname) = do
    content <- readFile filename
    eitherT print (saveToFiles rootoutputname) (processContentOfFile content)

processContentOfFile :: String -> EitherT String IO [String]
processContentOfFile content =
    case getMathElementsIndices content of
        [] -> throwError "No math elements in passed file"
        lst -> splitFileAt content lst

getMathElementsIndices :: String -> [(Int,Int)]
getMathElementsIndices content = zip (concat $ map (\x -> map (+ length x) (match (build x) content)) ["begin{math}","\\["]) (concat $ map (\x -> match (build x) content) ["\\end{math}","\\]"]) ++ (getIndicesFromShorthand content)

getIndicesFromShorthand :: String -> [(Int,Int)]
getIndicesFromShorthand content = getTuplesFromList $ match (build "$$") content

getTuplesFromList :: [Int] -> [(Int,Int)]
getTuplesFromList [] = []
getTuplesFromList (x1:x2:xs) = (x1+2,x2) : getTuplesFromList xs

splitFileAt :: String -> [(Int,Int)] -> EitherT String IO [String]
splitFileAt content lst = generateMLFromStrings $ splitFileAt' content lst

splitFileAt' :: String -> [(Int,Int)] -> [String]
splitFileAt' _ [] = []
splitFileAt' content ((x1,x2):xs) = take (x2-x1) (drop (x1) content) : splitFileAt' content xs

generateMLFromStrings :: [String] -> EitherT String IO [String]
generateMLFromStrings = mapM (\x -> scan x >>= parse >>= generate)

saveToFiles :: String -> [String] -> IO ()
saveToFiles rootName lst = saveToFiles' lst rootName 1

saveToFiles' :: [String] -> String -> Int -> IO ()
saveToFiles' [] _ _ = return ()
saveToFiles' (x:xs) fileRoot number = do
    saveToFile (fileRoot ++ show number ++ ".xhtml") x
    saveToFiles' xs fileRoot (number+1)

saveToFile :: String -> String -> IO ()
saveToFile filename content = do
    writeFile filename content
    print $ "File " ++ filename ++ " generated!"