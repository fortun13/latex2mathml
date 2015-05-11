module Latex2MathML.Scanner.Main (scan) where

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