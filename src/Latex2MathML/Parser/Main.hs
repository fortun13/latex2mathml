module Latex2MathML.Parser.Main (parse) where

import Latex2MathML.Utils.Definitions


parse :: [Token] -> [ASTModel]
parse [] = []

--TODO what about iint iiint etc.? MathML

