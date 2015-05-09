module Latex2MathML.Generator.Main where

import Latex2MathML.Utils.Definitions
import Data.Map
import Data.String
import System.IO
import System.Exit

generate :: [ASTModel] -> Either String [Char]
generate lst = return ("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<!DOCTYPE html  PUBLIC \"-//W3C//DTD XHTML 1.1 plus MathML 2.0//EN\" \n \"http://www.w3.org/Math/DTD/mathml2/xhtml-math11-f.dtd\">\n<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\">\n<head>\n<title>MathML Output File</title> \n </head> \n <body> \n <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n" ++ generate' lst ++  "\n</math>\n</body>\n</html>")

generate' :: [ASTModel] -> [Char]
generate' [] = ""
generate' ((ASTSub paramsSub):(ASTSup paramsSup):tail) = generateSubSup paramsSub paramsSup ++ generate' tail
generate' ((ASTSup paramsSub):(ASTSub paramsSup):tail) = generateSubSup paramsSup paramsSub ++ generate' tail
generate' (head:tail) = "<mrow>\n" ++ (generateFromAST head) ++ "</mrow>\n<hr></hr>\n" ++ generate' tail

generateSubSup :: [ASTModel] -> [ASTModel] -> [Char]
generateSubSup params1 params2 = "<mrow>\n<msubsup>\n<mi></mi>\n<mrow>\n" ++ generateRows params1 ++ "</mrow>\n<mrow>\n" ++ generateRows params2 ++ "</mrow>\n</msubsup>\n</mrow>\n"

generateRows :: [ASTModel] -> [Char]
generateRows [] = ""
generateRows (head:tail) = (generateFromAST head) ++ generateRows tail

generateFromAST :: ASTModel -> [Char]
generateFromAST (ComplexCommand name params body) = "<mtable>\n<mtr>\n<mtd>\n" ++ insertMTableBody body ++ "</mtd>\n</mtr>\n</mtable>\n" --TODO Alignment parameters for array?
generateFromAST (InlineCommand name params body) = "" -- Find out what kind of inline commands are defined - generator needs fixed list
generateFromAST (ASTSub body) = "<msub>\n<mi></mi>\n<mrow>\n" ++ generate' body ++ "</mrow>\n</msub>"
generateFromAST (ASTSup body) = "<msup>\n<mi></mi>\n<mrow>\n" ++ generate' body ++ "</mrow>\n</msup>"
generateFromAST (BodylessCommand commandName) = (translateSimpleCommandName commandName) ++ "\n"
generateFromAST (ASTOperator name) = "<mo>" ++ name ++ "</mo>\n"
generateFromAST (Variable value) = "<mi>" ++ [value] ++ "</mi>\n"
generateFromAST (MN value) = "<mn>" ++ value ++ "</mn>\n"
generateFromAST _ = ""

translateSimpleCommandName :: [Char] -> [Char]
translateSimpleCommandName name = (fromList (greekList ++ trigList)) ! name

insertMTableBody :: [ASTModel] -> [Char]
insertMTableBody [] = ""
insertMTableBody ((ASTOperator "&") : tail) = "</mtd>\n<mtd>" ++ insertMTableBody tail
insertMTableBody ((ASTOperator "\n") : tail) = "</mtd>\n</mtr>\n<mtr>\n<mtd>" ++ insertMTableBody tail
insertMTableBody (elem : tail) = generateFromAST elem ++ insertMTableBody tail

-- TODO Mapping for other simple commands, generating accents
greekList :: [(String, String)]
greekList = [("A","<mi>&Alpha;</mi>"), ("B","<mi>&Beta;</mi>"), ("Gamma","<mi>&Gamma;</mi>"), ("Delta","<mi>&Delta;</mi>"), ("E","<mi>&Epsilon;</mi>"), ("Z","<mi>&Zeta;</mi>"), ("H","<mi>&Eta;</mi>"), ("Theta","<mi>&Theta;</mi>"), ("I","<mi>&Iota;</mi>"), ("K","<mi>&Kappa;</mi>"), ("Lambda","<mi>&Lambda;</mi>"), ("M","<mi>&Mu;</mi>"), ("N","<mi>&Nu;</mi>"), ("Xi","<mi>&Xi;</mi>"), ("O","<mi>&Omicron;</mi>"), ("Pi","<mi>&Pi;</mi>"), ("P","<mi>&Rho;</mi>"), ("Sigma","<mi>&Sigma;</mi>"), ("T","<mi>&Tau;</mi>"), ("Upsilon","<mi>&Upsilon;</mi>"), ("Phi","<mi>&Phi;</mi>"), ("X","<mi>&Chi;</mi>"), ("Psi","<mi>&Psi;</mi>"), ("Omega","<mi>&Omega;</mi>"),
                         ("alpha","<mi>&alpha;</mi>"), ("beta","<mi>&beta;</mi>"), ("gamma","<mi>&gamma;</mi>"), ("delta","<mi>&delta;</mi>"), ("epsilon","<mi>&epsilon;</mi>"), ("zeta","<mi>&zeta;</mi>"), ("eta","<mi>&eta;</mi>"), ("theta","<mi>&theta;</mi>"), ("iota","<mi>&iota;</mi>"), ("kappa","<mi>&kappa;</mi>"), ("lambda","<mi>&lambda;</mi>"), ("mu","<mi>&mu;</mi>"), ("nu","<mi>&nu;</mi>"), ("xi","<mi>&xi;</mi>"), ("o","<mi>&omicron;</mi>"), ("pi","<mi>&pi;</mi>"), ("rho","<mi>&rho;</mi>"), ("sigma","<mi>&sigma;</mi>"), ("tau","<mi>&tau;</mi>"), ("upsilon","<mi>&upsilon;</mi>"), ("phi","<mi>&phi;</mi>"), ("chi","<mi>&chi;</mi>"), ("psi","<mi>&psi;</mi>"), ("omega","<mi>&omega;</mi>")]

trigList :: [(String, String)]
trigList = [("sin","<mi>sin</mi>"),("arcsin","<mi>arcsin</mi>"),("sinh","<mi>sinh</mi>"),("sec","<mi>sec</mi>"),("cos","<mi>cos</mi>"),("arccos","<mi>arccos</mi>"),("cosh","<mi>cosh</mi>"),("csc","<mi>csc</mi>"),("tan","<mi>tan</mi>"),("arctan","<mi>arctan</mi>"),("tanh","<mi>tanh</mi>"),("cot","<mi>cot</mi>"),("coth","<mi>coth</mi>")]

genmap :: Map String String
genmap = fromList [("A","<mi>&Alpha;</mi>"), ("B","<mi>&Beta;</mi>"), ("Gamma","<mi>&Gamma;</mi>"), ("Delta","<mi>&Delta;</mi>"), ("E","<mi>&Epsilon;</mi>"), ("Z","<mi>&Zeta;</mi>"), ("H","<mi>&Eta;</mi>"), ("Theta","<mi>&Theta;</mi>"), ("I","<mi>&Iota;</mi>"), ("K","<mi>&Kappa;</mi>"), ("Lambda","<mi>&Lambda;</mi>"), ("M","<mi>&Mu;</mi>"), ("N","<mi>&Nu;</mi>"), ("Xi","<mi>&Xi;</mi>"), ("O","<mi>&Omicron;</mi>"), ("Pi","<mi>&Pi;</mi>"), ("P","<mi>&Rho;</mi>"), ("Sigma","<mi>&Sigma;</mi>"), ("T","<mi>&Tau;</mi>"), ("Upsilon","<mi>&Upsilon;</mi>"), ("Phi","<mi>&Phi;</mi>"), ("X","<mi>&Chi;</mi>"), ("Psi","<mi>&Psi;</mi>"), ("Omega","<mi>&Omega;</mi>"),
    ("alpha","<mi>&alpha;</mi>"), ("beta","<mi>&beta;</mi>"), ("gamma","<mi>&gamma;</mi>"), ("delta","<mi>&delta;</mi>"), ("epsilon","<mi>&epsilon;</mi>"), ("zeta","<mi>&zeta;</mi>"), ("eta","<mi>&eta;</mi>"), ("theta","<mi>&theta;</mi>"), ("iota","<mi>&iota;</mi>"), ("kappa","<mi>&kappa;</mi>"), ("lambda","<mi>&lambda;</mi>"), ("mu","<mi>&mu;</mi>"), ("nu","<mi>&nu;</mi>"), ("xi","<mi>&xi;</mi>"), ("o","<mi>&omicron;</mi>"), ("pi","<mi>&pi;</mi>"), ("rho","<mi>&rho;</mi>"), ("sigma","<mi>&sigma;</mi>"), ("tau","<mi>&tau;</mi>"), ("upsilon","<mi>&upsilon;</mi>"), ("phi","<mi>&phi;</mi>"), ("chi","<mi>&chi;</mi>"), ("psi","<mi>&psi;</mi>"), ("omega","<mi>&omega;</mi>"),
    ("=","<mo>=</mo>"),("-","<mo>&minus;</mo>"), ("*","<mo>&times;</mo>"), ("div","<mo>&divide;</mo>"), ("neq","<mo>&ne;</mo>"), ("approx","<mo>&asymp;</mo>"), ("<","<mo>&lt;</mo>"), ("leq","<mo>&le;</mo>"), (">","<mo>&gt;</mo>"), ("geq","<mo>&ge;</mo>"), ("pm","<mo>&plusmn;</mo>"), ("propto","<mo>&prop;</mo>"), ("sum","<mo>&sum;</mo>"), ("prod","<mo>&prod;</mo>"), ("lfloor","<mo>&lfloor;</mo>"), ("rfloor","<mo>&rfloor;</mo>"), ("lceil","<mo>&lceil;</mo>"), ("rceil","<mo>&rceil;</mo>"),
    ("'","<mo>&prime;</mo>"), ("''","<mo>&Prime;</mo>"), ("'''","<mo>&tprime;</mo>"), ("''''","<mo>&qprime;</mo>"), ("partial","<mo>&part;</mo>"), ("Delta","<mo>&Delta;</mo>"), ("nabla","<mo>&Del;</mo>"), ("int","<mo>&int;</mo>"), ("iint","<mo>&Int;</mo>"), ("iiint","<mo>&tint;</mo>"), ("iiiint","<mo>&qint;</mo>"), ("oint","<mo>&conint;</mo>"), ("varointclockwise","<mo>&cwconint;</mo>"), ("ointctrclockwise","<mo>&awconint;</mo>"), ("oiint","<mo>&Conint;</mo>"), ("oiiint","<mo>&Cconint;</mo>"), ("infty","<mo>&infin;</mo>"),
    ("dots","<mo>&hellip;</mo>"), ("vdots","<mo>&vellip;</mo>"), ("cdots","<mo>&ctdot;</mo>"), ("udots","<mo>&utdot;</mo>"), ("ddots","<mo>&dtdot;</mo>")]
