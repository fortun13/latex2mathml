module Latex2MathML.Scanner.Main (scan) where

--TODO traktowanie pojedynczych spacji jako Operator "s"

import Data.Char (isDigit,isLetter)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
--import Data.Set (member)
import Latex2MathML.Utils.Definitions

scan :: String -> ([Token],String)
scan = tokenize

--prepareInput :: String -> String -> String
--prepareInput [] buffer = reverse buffer
--prepareInput lst@(h:t) buffer
--    | h == '%' = prepareInput (snd $ splitAt (fromJust (elemIndex '\n' lst) + 1) lst) buffer
----     | h == ' ' = prepareInput t buffer
--    | null t = reverse ('\n':h:buffer)
--    | otherwise = prepareInput t (h:buffer)

tokenize :: String -> ([Token],String)
tokenize [] = ([],[])
tokenize lst@(h:t)
    | h == '%' = tokenize (snd $ splitAt (fromJust (elemIndex '\n' lst) + 1) lst)
    | h == '\n' && t /= [] = scan t
    | h == '\\' = iterateOver readCommand t
    | h == ' ' || h == '\n' = tokenize t
    | h == '^' = addToken Sup t
    | h == '_' = addToken Sub t
    | isDigit h = iterateOver readNumber lst
    | h `elem` operators = iterateOver readOperator lst
    | isLetter h = iterateOver readString lst
    | h == '{' = addToken BodyBegin t
    | h == '}' = addToken BodyEnd t
    | otherwise = ([],lst)

addToken :: Token -> String -> ([Token],String)
addToken type' lst =
    let tmp = tokenize lst
    in ([type'] ++ fst tmp, snd tmp)

iterateOver :: (t -> String -> (Token,String)) -> t -> ([Token],String)
iterateOver function lst = (fst tmp : fst tmp2,snd tmp2)
    where tmp = function lst ""
          tmp2 = tokenize (snd tmp)

readOperator :: String -> String -> (Token,String)
readOperator [] [] = (End,[])
readOperator [] buffer = (Operator $ reverse buffer,[])
readOperator (h:t) ""
    | h == '\'' = readOperator t [h]
    | otherwise = (Operator [h],t)
readOperator lst@(h:t) buffer
    | h == '\'' = readOperator t (h:buffer)
    | otherwise = (Operator $ reverse buffer,lst)

readString :: String -> String -> (Token,String)
readString [] [] = (End,[])
readString [] buffer = (MyStr $ reverse buffer,[])
readString (h:t) ""
    | h `elem` "ABEZHIKMNOoTX" = (Command [h],t)
readString lst@(h:t) buffer
    | h `elem` "ABEZHIKMNOoTX" = (MyStr $ reverse buffer,lst)
    | isLetter h = readString t (h:buffer)
    | h == ' ' = readString t buffer
    | otherwise = (MyStr $ reverse buffer,lst)

readNumber :: String -> String -> (Token,String)
readNumber [] [] = (End,[])
readNumber [] buffer = (MyNum $ reverse buffer,[])
readNumber lst@(h:t) buffer
    | isDigit h || h == '.' = readNumber t (h:buffer)
    | h == ' ' = readNumber t buffer
    | otherwise = (MyNum $ reverse buffer,lst)

readCommand :: String -> String -> (Token,String)
readCommand [] [] = (End,[])
readCommand [] buffer = (Command $ reverse buffer,[])
readCommand (h:t) ""
    | h `elem` "{}[]()" = (Operator [h],t)
    | h == '\\' = (Operator "\n",t)
    | h == ' ' = (Operator "s",t)
    | otherwise = readCommand t [h]
readCommand lst@(h:t) buffer
    | isLetter h = readCommand t (h:buffer)
    | otherwise = (Command $ reverse buffer,lst)

operators :: String
operators = "+-*/=!():<>|[]&\n,.'$"

--tokenize :: String -> Char -> ([Token],String)
--tokenize [] _ = ([],[])
--tokenize lst@(h:t) stopSign
--    | h == '\n' && t /= [] = scan t
--    | h == stopSign = ([],t)
--    | h == '\\' = iterateOver readCommand t stopSign
--    | h == ' ' || h == '\n' = tokenize t stopSign
--    | h == '^' = iterateOver readSup t stopSign
--    | h == '_' = iterateOver readSub t stopSign
--    | isDigit h = iterateOver readNumber lst stopSign
--    | h `elem` operators = iterateOver readOperator lst stopSign
----        let tmp = tokenize t stopSign
----        in (Operator [h] : fst tmp,snd tmp)
--    | isLetter h = iterateOver readString lst stopSign
--    -- TODO i am not sure it should be here (need more test cases)
--    | h == '}' || h == '{' = tokenize t stopSign
--    | otherwise = ([],lst)
--
--iterateOver :: (t -> String -> (Token,String)) -> t -> Char -> ([Token],String)
--iterateOver function lst stopSign
--    | fst tmp == ComplexEnd = ([],snd tmp)
--    | otherwise = (fst tmp : fst tmp2,snd tmp2)
--    where tmp = function lst ""
--          tmp2 = tokenize (snd tmp) stopSign
--
--readCommand :: String -> String -> (Token,String)
--readCommand [] [] = (End,[])
--readCommand [] buffer = (CommandBodyless $ reverse buffer,[])
--readCommand (h:t) ""
--    | h `elem` "{}[]()" = (Operator [h],t)
--    | h == '\\' = (Operator "\n",t)
--    | h == ' ' = (Operator "s",t)
--    | otherwise = readCommand t [h]
--readCommand lst@(h:t) buffer
--    | null lst || (h `elem` " }()_^\\" && buffer /= "carf") = (CommandBodyless $ reverse buffer,lst)
--    | (h == '{' || h == '[') && ("nigeb" == buffer) = readComplexCommand lst
--    | h == '{' && ("dne" == buffer) = (ComplexEnd,snd $ splitAt (fromJust (elemIndex '}' lst)+1) lst)
--    | h == '{' || h == '[' = readInlineCommand (reverse buffer) lst
--    | buffer == "carf" =
--        if isDigit h then
--            let (h1:h2:t2) = lst
--            in (InlineCommand (reverse buffer) [] [[MyNum [h1]],[MyNum [h2]]],t2)
--        else if h == ' ' then
--            let (h1:h2:h3:h4:t2) = lst
--            in (InlineCommand (reverse buffer) [] [[MyNum [h2]],[MyNum [h4]]],t2)
--        -- if frac has body we should never be here, because h == '{' will catch it earlier
--        else (End,[])
--    | h == '\n' = readCommand t buffer
--    | h `elem` operators = (CommandBodyless $ reverse buffer,lst)
--    | otherwise = readCommand t (h:buffer)
--
--readCommandBody :: String -> ([[Token]],String)
--readCommandBody ('{':t) =
--    let tmp = tokenize t '}'
--        tmp2 = readCommandBody (snd tmp)
--    in (fst tmp : fst tmp2,snd tmp2)
--readCommandBody lst = ([],lst)
--
--readInlineCommand :: String -> String -> (Token,String)
--readInlineCommand name lst =
--    let par = readParameters lst
--        body = readCommandBody (snd par)
--    in (InlineCommand name (fst par) (fst body),snd body)
--
--readParameters :: String -> ([Token],String)
--readParameters ('[':t) = tokenize t ']'
--readParameters lst = ([],lst)
--
--readComplexCommand :: String -> (Token,String)
--readComplexCommand [] = (End,[])
--readComplexCommand lst
--    | isComplexCommand commName =
--        let par = readComplexParameters rest
--            tmp = tokenize (snd par) '}'
--        in (ComplexCommand commName (fst par) (fst tmp),snd tmp)
--    where ([MyStr commName],rest) = readComplexCommandName lst
--
--readComplexCommandName :: String -> ([Token],String)
--readComplexCommandName lst = tokenize lst '}'
--
--readComplexParameters :: String -> ([Token],String)
--readComplexParameters (h:t)
--    | h == '[' =
--        let tmp = tokenize t ']'
--            tmp2 = readComplexParameters (snd tmp)
--        in (fst tmp ++ fst tmp2, snd tmp2)
--    | h == '{' =
--        let tmp = tokenize t '}'
--            tmp2 = readComplexParameters (snd tmp)
--        in (fst tmp ++ fst tmp2, snd tmp2)
--readComplexParameters lst = ([],lst)
--
--isComplexCommand :: String -> Bool
--isComplexCommand comm = comm `elem` ["matrix","table","array"]
--
--readSup :: String -> String -> (Token,String)
--readSup lst _ = readSupOrSub lst Sup
--
--readSub :: String -> String -> (Token,String)
--readSub lst _ = readSupOrSub lst Sub
--
--readSupOrSub lst@(h:t) type'
--    | h == '{' = runTokenizer t '}' [] type'
--    | h == '\\' = runTokenizer lst ' ' [] type'
--    | otherwise = runTokenizer [h] ' ' t type'
--
--runTokenizer :: String -> Char -> String -> ([Token] -> Token) -> (Token,String)
--runTokenizer lst stopSign [] type' =
--    let tmp = tokenize lst stopSign
--    in (type' (fst tmp),snd tmp)
--runTokenizer lst stopSign returnList type' =
--    let tmp = tokenize lst stopSign
--    in (type' (fst tmp),returnList)