module Latex2MathML.Parser.Main (parse) where

import Latex2MathML.Utils.Definitions

parse :: [Token] -> [Token]
parse [] = []
parse (h:t)
    | checkCorrectness h = [h] ++ parse t
    | otherwise = [Error] ++ t

checkCorrectness :: Token -> Bool
checkCorrectness (CommandBodyless cmd) = cmd `elem` ["alpha","beta","A","B"]
checkCorrectness (InlineCommand name parameters bodies) = name `elem` ["frac"]
    && all checkCorrectness parameters
    && all (==True) (map (\lst -> all checkCorrectness lst) bodies)
checkCorrectness (ComplexCommand name parameters body) = name `elem` ["matrix"]
    && all checkCorrectness parameters
    && all checkCorrectness body
checkCorrectness (Sub body) = all checkCorrectness body
checkCorrectness (Sup body) = all checkCorrectness body
checkCorrectness _ = True