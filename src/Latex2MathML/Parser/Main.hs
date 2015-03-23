module Latex2MathML.Parser.Main (parse) where

import Latex2MathML.Utils.Definitions
import Data.Set (fromList,member,Set)

parse :: [Token] -> [Token]
parse [] = []
parse (h:t)
    | checkCorrectness h = h : parse t
    | otherwise = Error : t

checkCorrectness :: Token -> Bool
checkCorrectness (CommandBodyless cmd) = member cmd bodyless
checkCorrectness (InlineCommand name parameters bodies) = member name inline
    && all checkCorrectness parameters
    && all (==True) (map (all checkCorrectness) bodies)
checkCorrectness (ComplexCommand name parameters body) = member name complex
    && all checkCorrectness parameters
    && all checkCorrectness body
checkCorrectness (Sub body) = all checkCorrectness body
checkCorrectness (Sup body) = all checkCorrectness body
checkCorrectness _ = True

bodyless :: Set String
bodyless = fromList ["alpha","beta","A","B"]

inline :: Set String
inline = fromList ["frac","sqrt","binom"]

complex :: Set String
complex = fromList ["matrix","table","array"]