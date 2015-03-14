module Scanner.Scan2 (scan) where

import Data.Char (isDigit,isLetter,toLower,isUpper)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Scanner.Definitions

-- numebers will be part of strings (same with operators) (same with brackets)

scan :: String -> [Token]
scan lst@(h:t)
    | h == '\\' = useCommand (readCommand) t
    | h == ' ' || h == '\n' = scan t
    | h == '^' = useCommand (readSup) t
    | h == '_' = useCommand (readSub) t
    | otherwise = useCommand (readString) t

useCommand command lst =
    let tmp = command lst []
    in [fst tmp] ++ scan (snd tmp)

readCommand :: String -> String -> (Token,String)
readCommand [] [] = (End,[])

readString :: String -> String -> (Token,String)
readString [] [] = (MyStr "",[])

readSub :: String -> String -> (Token,String)
readSub [] [] = (Sub [],[])

readSup :: String -> String -> (Token,String)
readSup [] [] = (Sup [],[])


