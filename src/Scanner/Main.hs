module Scanner.Main (scan) where

import Data.Char (isDigit,isLetter,toLower,isUpper)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Scanner.Definitions

--TODO [] - meaning is dependend on the context :( (so, i guess, in scanner it should only be in SquareBracket or smth (and parser would take care of using it properly)
--TODO same with & (used in tables, matrices etc.)

--TODO problem with [] - i should be using it in readString, but somehow i have to know if i'm using readParameters

--TODO comments not working

scan :: String -> Char -> ([Token],String)
scan [] _ = ([],[])
scan lst@(h:t) stopSign
    | h == stopSign = ([],t)
    | h == '\\' = iterateOver (readCommand) t stopSign
    | h == ' ' || h == '\n' = scan t stopSign
--    | h == '{' = iterateOver readCommandBody lst stopSign
    | h == '^' = iterateOver readSup t stopSign
    | h == '_' = iterateOver readSub t stopSign
    | h == '%' = scan (snd (splitAt ((fromJust (elemIndex '\n' lst))+1) lst)) stopSign
    | lst == [] = ([],[])
--    | otherwise = iterateOver readString lst stopSign
    | otherwise = iterateOver readString lst stopSign

iterateOver function lst stopSign =
    let tmp = function lst ""
        tmp2 = scan (snd tmp) stopSign
    in ([fst tmp] ++ fst tmp2,snd tmp2)

readString :: String -> String -> (Token,String)
readString [] [] = (End,[])
readString [] buffer = (MyStr $ reverse buffer,[])
readString lst@(h:t) buffer
    | isLetter h || isDigit h || elem h "+-*/=!<>:|()." = readString t (h:buffer)
    | otherwise = (MyStr $ reverse buffer,lst)

readCommand :: String -> String -> (Token,String)
readCommand [] [] = (End,[])
readCommand [] buffer = (createBodylessCommand $ reverse buffer,[])
readCommand lst@(h:t) buffer
    | lst == [] || h == ' ' = (createBodylessCommand $ reverse buffer,lst)
    | (h == '{' || h == '[') && (isComplexCommand $ reverse buffer) = readComplexCommand (reverse buffer) lst
    | (h == '{' || h == '[') = readInlineCommand (reverse buffer) lst
    | otherwise = readCommand t (h:buffer)

createBodylessCommand :: String -> Token
createBodylessCommand comm
    | isGreekCommand comm = CommandBodyless $ getGreekByName comm

isGreekCommand :: String -> Bool
isGreekCommand comm
    | elem (map (\x -> toLower x) comm) ["alpha","beta","gamma"] = True
    | otherwise = False

getGreekByName :: String -> Bodylesstype
getGreekByName comm@(h:t) = Greek (getGreekSymbol ((toLower h):t)) (isUpper h)

getGreekSymbol :: String -> GreekSymbol
getGreekSymbol comm
    | comm == "alpha" = Alpha
    | comm == "beta" = Beta
    | comm == "gamma" = Gamma

readCommandBody :: String -> ([[Token]],String)
readCommandBody [] = ([],[])
readCommandBody ('{':t) =
    let tmp = scan t '}'
        tmp2 = readCommandBody (snd tmp)
    in ([fst tmp] ++ fst tmp2,snd tmp2)
readCommandBody lst = ([],lst)

-- commandName restOfInput Buffer
readInlineCommand :: String -> String -> (Token,String)
readInlineCommand name lst = readCommandWithParameters name lst InlineCommand

readParameters :: String -> ([Token],String)
readParameters [] = ([],[])
readParameters lst@(h:t)
    | h == '[' = scan lst ']'
    | otherwise = ([],lst)

readComplexCommand :: String -> String -> (Token,String)
readComplexCommand name lst = readCommandWithParameters name lst ComplexCommand

readCommandWithParameters _ [] _ = (End,[])
readCommandWithParameters name lst type' =
    let par = readParameters lst
        body = readCommandBody (snd par)
    in  (type' name (fst par) (fst body),snd body)
--    | h == '[' =
--        let par = readParameters t
--            body = readCommandBody (snd par)
--        in  (type' name (fst par) (fst body),snd body)
--    | otherwise =
--        let body = readCommandBody lst
--        in (type' name [] ())

isComplexCommand :: String -> Bool
isComplexCommand comm = elem comm ["matrix","table","array"]

readSup :: String -> String -> (Token,String)
readSup lst buffer = readSupOrSub lst Sup

readSub :: String -> String -> (Token,String)
readSub lst buffer = readSupOrSub lst Sub

readSupOrSub :: String -> ([Token] -> Token) -> (Token,String)
readSupOrSub [] _ = (End,[])
readSupOrSub (h:[]) type' =
    let tmp = scan [h] ' '
    in (type' (fst tmp),[])
readSupOrSub lst@(h:h2:t) type'
    | h == '{' =
        let tmp = scan (h2:t) '}'
        in (type' (fst tmp),snd tmp)
    | h == '\\' =
        let tmp = scan lst ' '
        in (type' (fst tmp),snd tmp)
    | otherwise =
        let tmp = scan [h] ' '
        in (type' (fst tmp),(h2:t))

