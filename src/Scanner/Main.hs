module Scanner.Main (scan) where

import Data.Char (isDigit,isLetter,toLower,isUpper)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Scanner.Definitions

--TODO [] - meaning is dependend on the context :( (so, i guess, in scanner it should only be in SquareBracket or smth (and parser would take care of using it properly)
--TODO same with & (used in tables, matrices etc.)

--TODO problem with [] - i should be using it in readString, but somehow i have to know if i'm using readParameters

scan :: String -> ([Token],String)
scan lst = flip tokenize '\n' (prepareInput lst "")

prepareInput :: String -> String -> String
prepareInput [] buffer = reverse buffer
prepareInput lst@(h:t) buffer
    | h == '%' = prepareInput (snd $ splitAt ((fromJust (elemIndex '\n' lst))+1) lst) buffer
    | h == ' ' = prepareInput t buffer
    | t == [] = reverse ('\n':h:buffer)
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
    | elem h "+-*/=!():<>|[]&\n" =
        let tmp = tokenize t stopSign
        in ([Operator h] ++ (fst tmp),snd tmp)
    | otherwise = iterateOver readString lst stopSign

iterateOver :: (t -> String -> (Token,String)) -> t -> Char -> ([Token],String)
iterateOver function lst stopSign
    | (fst tmp) == ComplexEnd = ([],snd tmp)
    | otherwise = ([fst tmp] ++ fst tmp2,snd tmp2)
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
readCommand [] buffer = (createBodylessCommand $ reverse buffer,[])
readCommand lst@(h:t) buffer
    | lst == [] || h == ' ' = (createBodylessCommand $ reverse buffer,lst)
    | (h == '{' || h == '[') && ("begin" == reverse buffer) = readComplexCommand lst
    | h == '{' && ("end" == reverse buffer) = (ComplexEnd,snd $ splitAt ((fromJust (elemIndex '}' lst))+1) lst)
    | (h == '{' || h == '[') = readInlineCommand (reverse buffer) lst
    | h == '\\' = (Operator '\n',t)
    | otherwise = readCommand t (h:buffer)

createBodylessCommand :: String -> Token
createBodylessCommand comm
    | isGreekCommand comm = CommandBodyless $ getGreekByName comm

isGreekCommand :: String -> Bool
isGreekCommand comm
    | elem (map (\x -> toLower x) comm) ["alpha","beta","gamma"] = True
    | otherwise = False

getGreekByName :: String -> Bodylesstype
getGreekByName (h:t) = Greek (getGreekSymbol ((toLower h):t)) (isUpper h)

getGreekSymbol :: String -> GreekSymbol
getGreekSymbol comm
    | comm == "alpha" = Alpha
    | comm == "beta" = Beta
    | comm == "gamma" = Gamma

readCommandBody :: String -> ([[Token]],String)
readCommandBody ('{':t) =
    let tmp = tokenize t '}'
        tmp2 = readCommandBody (snd tmp)
    in ([fst tmp] ++ fst tmp2,snd tmp2)
readCommandBody lst = ([],lst)

-- commandName restOfInput Buffer
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
isComplexCommand comm = elem comm ["matrix","table","array"]

readSup :: String -> String -> (Token,String)
readSup lst _ = readSupOrSub lst Sup

readSub :: String -> String -> (Token,String)
readSub lst _ = readSupOrSub lst Sub

readSupOrSub :: String -> ([Token] -> Token) -> (Token,String)
readSupOrSub [] _ = (End,[])
readSupOrSub (h:[]) type' =
    let tmp = tokenize [h] ' '
    in (type' (fst tmp),[])
readSupOrSub lst@(h:h2:t) type'
    | h == '{' =
        let tmp = tokenize (h2:t) '}'
        in (type' (fst tmp),snd tmp)
    | h == '\\' =
        let tmp = tokenize lst ' '
        in (type' (fst tmp),snd tmp)
    | otherwise =
        let tmp = tokenize [h] ' '
        in (type' (fst tmp),(h2:t))

