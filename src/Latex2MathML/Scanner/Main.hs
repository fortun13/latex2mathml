module Latex2MathML.Scanner.Main (scan) where

--TODO traktowanie pojedynczych spacji jako Operator "s"

import Data.Char (isDigit,isLetter)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Set (member)
import Latex2MathML.Utils.Definitions
import Latex2MathML.Utils.Functions (throwError)

scan :: String -> Either String [Token]
scan source =
    case tokenize source 1 of
        Right (lst,_,_) -> Right lst
        Left msg -> Left msg


--prepareInput :: String -> String -> String
--prepareInput [] buffer = reverse buffer
--prepareInput lst@(h:t) buffer
--    | h == '%' = prepareInput (snd $ splitAt (fromJust (elemIndex '\n' lst) + 1) lst) buffer
----     | h == ' ' = prepareInput t buffer
--    | null t = reverse ('\n':h:buffer)
--    | otherwise = prepareInput t (h:buffer)

tokenize :: String -> Integer -> Either String ([Token],String,Integer)
tokenize [] num = return ([],[],num)
tokenize lst@(h:t) num
    | h == '%' = tokenize (snd $ splitAt (fromJust (elemIndex '\n' lst) + 1) lst) num
    | h == '\n' && t /= [] = tokenize t (num+1)
    | h == '\\' = iterateOver readCommand t num
    | h == ' ' || h == '\n' = tokenize t num
    | h == '^' = addToken Sup t num
    | h == '_' = addToken Sub t num
    | isDigit h = iterateOver readNumber lst num
    | h `elem` operators = iterateOver readOperator lst num
    | isLetter h = iterateOver readString lst num
    | h == '{' = addToken BodyBegin t num
    | h == '}' = addToken BodyEnd t num
    | otherwise = throwError $ "Character not matched: " ++ [h] ++ " before: " ++ take 20 t ++ " line: " ++ show num

addToken :: Token -> String -> Integer -> Either String ([Token],String,Integer)
addToken type' lst num = (tokenize lst num) >>= (\x -> return ([type'] ++ fst' x,snd' x, thr' x))

iterateOver :: (t -> String -> Integer -> Either String (Token,String)) -> t -> Integer -> Either String ([Token],String,Integer)
--iterateOver function lst num = (function lst "" num) >>= (\x -> tokenize (snd x) num) >>= (\x -> return (fst' x, snd' x, thr' x))
iterateOver function lst num = do
    tmp <- function lst "" num
    tmp2 <- tokenize (snd tmp) num
    return (fst tmp : fst' tmp2, snd' tmp2, thr' tmp2)

readOperator :: String -> String -> Integer -> Either String (Token,String)
readOperator [] [] _ = return (End,[])
readOperator [] buffer _ = return (Operator $ reverse buffer,[])
readOperator (h:t) "" n
    | h == '\'' = readOperator t [h] n
    | otherwise = return (Operator [h],t)
readOperator lst@(h:t) buffer n
    | h == '\'' = readOperator t (h:buffer) n
    | otherwise = return (Operator $ reverse buffer,lst)

readString :: String -> String -> Integer -> Either String (Token,String)
readString [] [] _ = return (End,[])
readString [] buffer _ = return (MyStr $ reverse buffer,[])
readString (h:t) "" _
    | h `elem` "ABEZHIKMNOoTX" = return (Command [h],t)
readString lst@(h:t) buffer n
    | h `elem` "ABEZHIKMNOoTX" = return (MyStr $ reverse buffer,lst)
    | isLetter h = readString t (h:buffer) n
    | h == ' ' = readString t buffer n
    | otherwise = return (MyStr $ reverse buffer,lst)

readNumber :: String -> String -> Integer -> Either String (Token,String)
readNumber [] [] _ = return (End,[])
readNumber [] buffer _ = return (MyNum $ reverse buffer,[])
readNumber lst@(h:t) buffer n
    | isDigit h || h == '.' = readNumber t (h:buffer) n
    | h == ' ' = readNumber t buffer n
    | otherwise = return (MyNum $ reverse buffer,lst)

readCommand :: String -> String -> Integer -> Either String (Token,String)
readCommand [] [] _ = return (End,[])
readCommand [] buffer line = do
    cmd <- getCommand [] (reverse buffer) line
    return (cmd,[])
readCommand (h:t) "" l
    | h `elem` "|" = return (Command "doubleOr",t)
    | h `elem` "{}[]()" = return (Operator [h],t)
    | h == '\\' = return (Operator "\n",t)
    | h == ' ' = return (Operator "s",t)
    | otherwise = readCommand t [h] l
readCommand lst@(h:t) buffer line
    | h `elem` "[]()" && (buffer == "tfel" || buffer == "thgir") = return (Command $ reverse $ h:buffer,t)
    | h == '\\' && (buffer == "tfel" || buffer == "thgir") =
        let tmp = head t
        in
            if (tmp == '{' || tmp == '}')
                then return (Command $ reverse $ tmp:buffer,tail t)
                else throwError $ "Unrecognized pattern: " ++ reverse (tmp:h:buffer)
    | isLetter h = readCommand t (h:buffer) line
    | otherwise = do
        cmd <- getCommand lst (reverse buffer) line
        return (cmd, lst)

--TODO validate complex command name
getCommand :: String -> String -> Integer -> Either String Token
getCommand _ "" line = throwError $ "No idea... line: " ++ show line
getCommand lst name line
    | member name commands == True = return $ Command name
    | name == "begin" || name == "end" =
        if readComplexCommandName lst "" == True
        then return $ Command name
        else throwError $ "Command not recognized around: " ++ name ++ show (take 20 lst) ++ " line: " ++ show line
    | otherwise = throwError $ "Command not recognized: " ++ name ++ " line: " ++ show line

readComplexCommandName :: String -> String -> Bool
readComplexCommandName [] _ = False
readComplexCommandName (h:t) buffer
    | h == '{' = readComplexCommandName t buffer
    | h == '}' = member (reverse buffer) complex
    | isLetter h = readComplexCommandName t (h : buffer)
    | otherwise = False

fst' :: (a,b,c) -> a
fst' (x,_,_) = x

snd' :: (a,b,c) -> b
snd' (_,x,_) = x

thr' :: (a,b,c) -> c
thr' (_,_,x) = x

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