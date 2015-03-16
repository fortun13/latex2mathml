module Scanner.Main (scan) where

import Data.Char (isDigit,isLetter)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Scanner.Definitions

scan :: String -> ([Token],String)
scan lst = tokenize (prepareInput lst "") '\n'

prepareInput :: String -> String -> String
prepareInput [] buffer = reverse buffer
prepareInput lst@(h:t) buffer
    | h == '%' = prepareInput (snd $ splitAt (fromJust (elemIndex '\n' lst) + 1) lst) buffer
--     | h == ' ' = prepareInput t buffer
    | null t = reverse ('\n':h:buffer)
    | otherwise = prepareInput t (h:buffer)

tokenize :: String -> Char -> ([Token],String)
tokenize [] _ = ([],[])
tokenize lst@(h:t) stopSign
    | h == stopSign = ([],t)
    | h == '\\' = iterateOver readCommand t stopSign
    | h == ' ' || h == '\n' = tokenize t stopSign
    | h == '^' = iterateOver readSup t stopSign
    | h == '_' = iterateOver readSub t stopSign
    | isDigit h = iterateOver readNumber lst stopSign
    | h `elem` "+-*/=!():<>|[]&\n" =
        let tmp = tokenize t stopSign
        in (Operator h : fst tmp,snd tmp)
    | otherwise = iterateOver readString lst stopSign

iterateOver :: (t -> String -> (Token,String)) -> t -> Char -> ([Token],String)
iterateOver function lst stopSign
    | fst tmp == ComplexEnd = ([],snd tmp)
    | otherwise = (fst tmp : fst tmp2,snd tmp2)
    where tmp = function lst ""
          tmp2 = tokenize (snd tmp) stopSign

readString :: String -> String -> (Token,String)
readString [] [] = (End,[])
readString [] buffer = (MyStr $ reverse buffer,[])
readString lst@(h:t) buffer
    | isLetter h = readString t (h:buffer)
    | otherwise = (MyStr $ reverse buffer,lst)

readNumber :: String -> String -> (Token,String)
readNumber [] [] = (End,[])
readNumber [] buffer = (MyNum $ reverse buffer,[])
readNumber lst@(h:t) buffer
    | isDigit h || h == '.' = readNumber t (h:buffer)
    | otherwise = (MyNum $ reverse buffer,lst)

readCommand :: String -> String -> (Token,String)
readCommand [] [] = (End,[])
readCommand [] buffer = (CommandBodyless $ reverse buffer,[])
readCommand lst@(h:t) buffer
    | null lst || h == ' ' = (CommandBodyless $ reverse buffer,lst)
    | (h == '{' || h == '[') && ("begin" == reverse buffer) = readComplexCommand lst
    | h == '{' && ("end" == reverse buffer) = (ComplexEnd,snd $ splitAt (fromJust (elemIndex '}' lst)+1) lst)
    | h == '{' || h == '[' = readInlineCommand (reverse buffer) lst
    | h == '\\' = (Operator '\n',t)
    | otherwise = readCommand t (h:buffer)

readCommandBody :: String -> ([[Token]],String)
readCommandBody ('{':t) =
    let tmp = tokenize t '}'
        tmp2 = readCommandBody (snd tmp)
    in (fst tmp : fst tmp2,snd tmp2)
readCommandBody lst = ([],lst)

readInlineCommand :: String -> String -> (Token,String)
readInlineCommand name lst =
    let par = readParameters lst
        body = readCommandBody (snd par)
    in (InlineCommand name (fst par) (fst body),snd body)

readParameters :: String -> ([Token],String)
readParameters ('[':t) = tokenize t ']'
readParameters lst = ([],lst)

readComplexCommand :: String -> (Token,String)
readComplexCommand [] = (End,[])
readComplexCommand lst
    | isComplexCommand commName =
        let par = readParameters rest
            tmp = tokenize (snd par) '}'
        in (ComplexCommand commName (fst par) (fst tmp),snd tmp)
    where ([[MyStr commName]],rest) = readCommandBody lst

isComplexCommand :: String -> Bool
isComplexCommand comm = comm `elem` ["matrix","table","array"]

readSup :: String -> String -> (Token,String)
readSup lst _ = readSupOrSub lst Sup

readSub :: String -> String -> (Token,String)
readSub lst _ = readSupOrSub lst Sub

readSupOrSub :: String -> ([Token] -> Token) -> (Token,String)
readSupOrSub [] _ = (End,[])
readSupOrSub lst@(h:t) type'
    | h == '{' = runTokenize t '}' type'
    | h == '\\' = runTokenize lst ' ' type'
    | otherwise =
        let tmp = tokenize [h] ' '
        in (type' (fst tmp),t)

runTokenize :: String -> Char -> ([Token] -> Token) -> (Token,String)
runTokenize lst stopSign type' =
    let tmp = tokenize lst stopSign
    in (type' (fst tmp),snd tmp)