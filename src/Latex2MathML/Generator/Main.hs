module Latex2MathML.Generator.Main(generate) where

import Latex2MathML.Utils.Definitions
import Data.Map
import Control.Monad.Trans.Either

generate :: [ASTModel] -> EitherT String IO String
generate list = return ("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<!DOCTYPE html  PUBLIC \"-//W3C//DTD XHTML 1.1 plus MathML 2.0//EN\" \n \"http://www.w3.org/Math/DTD/mathml2/xhtml-math11-f.dtd\">\n<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\">\n<head>\n<title>MathML Output File</title> \n </head> \n <body> \n <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n" ++ generate' list ++  "\n</math>\n</body>\n</html>")

generate' :: [ASTModel] -> [Char]
generate' [] = ""
generate' ((BodylessCommand name):(ASTSub subArgs):(MN notImportant):(ASTSup supArgs):rest)
    | name `elem` productionNames = generateUnderOver name subArgs supArgs ++ generate' rest
generate' ((BodylessCommand name):(ASTSup supArgs):(MN notImportant):(ASTSub subArgs):rest)
    | name `elem` productionNames = generateUnderOver name subArgs supArgs ++ generate' rest
generate' ((BodylessCommand name):(ASTSub subArgs):(ASTSup supArgs):rest)
    | name `elem` productionNames = generateUnderOver name subArgs supArgs ++ generate' rest
generate' ((BodylessCommand name):(ASTSup supArgs):(ASTSub subArgs):rest)
    | name `elem` productionNames = generateUnderOver name subArgs supArgs ++ generate' rest
generate' ((BodylessCommand name):(ASTSub subArgs):rest)
    | name `elem` productionNames = generateUnder name subArgs ++ generate' rest
generate' ((BodylessCommand name):(ASTSup supArgs):rest)
    | name `elem` productionNames = generateOver name supArgs ++ generate' rest
generate' ((ASTSub subArgs):(MN notImportant):(ASTSup supArgs):rest) = generateSubSup subArgs supArgs ++ generate' rest
generate' ((ASTSup supArgs):(ASTSub subArgs):(MN notImportant):rest) = generateSubSup subArgs supArgs ++ generate' rest
generate' ((ASTSub subArgs):(ASTSup supArgs):rest) = generateSubSup subArgs supArgs ++ generate' rest
generate' ((ASTSup supArgs):(ASTSub subArgs):rest) = generateSubSup subArgs supArgs ++ generate' rest
generate' (firstElement:rest) = "<mrow>\n" ++ (generateFromASTElem firstElement) ++ "</mrow>\n<hr></hr>\n" ++ generate' rest

generateSubSup :: [ASTModel] -> [ASTModel] -> [Char]
generateSubSup subArgs supArgs = "<mrow>\n<msubsup>\n<mi></mi>\n<mrow>\n" ++ generate' subArgs ++ "</mrow>\n<mrow>\n" ++ generate' supArgs ++ "</mrow>\n</msubsup>\n</mrow>\n"

generateFromASTLis :: [ASTModel] -> [Char]
generateFromASTLis [] = ""
generateFromASTLis (firstElement:rest) = (generateFromASTElem firstElement) ++ generate' rest

generateUnderOver :: String -> [ASTModel] -> [ASTModel] -> [Char]
generateUnderOver name subArgs supArgs = "<mrow>\n<munderover>\n" ++ (fromList otherTransList) ! name ++ "\n<mrow>\n" ++ generate' subArgs ++ "</mrow>\n<mrow>\n" ++ generate' supArgs ++ "</mrow>\n</munderover>\n</mrow>\n"

generateUnder :: String -> [ASTModel] -> [Char]
generateUnder name subArgs = "<mrow>\n<munder>\n" ++ (fromList otherTransList) ! name ++ "\n<mrow>\n" ++ generate' subArgs ++ "</mrow>\n</munder>\n</mrow>\n"

generateOver :: String -> [ASTModel] -> [Char]
generateOver name supArgs = "<mrow>\n<mover>\n" ++ (fromList otherTransList) ! name ++ "\n<mrow>\n" ++ generate' supArgs ++ "</mrow>\n</mover>\n</mrow>\n"

generateFromASTElem :: ASTModel -> [Char]
generateFromASTElem (ComplexCommand name params body)
    | name == "pmatrix" = "<mfenced open='(' close=')' separators=''>\n<mtable>\n<mtr>\n<mtd>\n" ++ insertMTableBody body ++ "</mtd>\n</mtr>\n</mtable>\n</mfenced>\n"
    | name == "bmatrix" = "<mfenced open='[' close=']' separators=''>\n<mtable>\n<mtr>\n<mtd>\n" ++ insertMTableBody body ++ "</mtd>\n</mtr>\n</mtable>\n</mfenced>\n"
    | name == "Bmatrix" = "<mfenced open='{' close='}' separators=''>\n<mtable>\n<mtr>\n<mtd>\n" ++ insertMTableBody body ++ "</mtd>\n</mtr>\n</mtable>\n</mfenced>\n"
    | name == "vmatrix" = "<mfenced open='|' close='|' separators=''>\n<mtable>\n<mtr>\n<mtd>\n" ++ insertMTableBody body ++ "</mtd>\n</mtr>\n</mtable>\n</mfenced>\n"
    | name == "Vmatrix" = "<mfenced open='&spar;' close='&spar;' separators=''>\n<mtable>\n<mtr>\n<mtd>\n" ++ insertMTableBody body ++ "</mtd>\n</mtr>\n</mtable>\n</mfenced>\n"
    | otherwise = "<mtable>\n<mtr>\n<mtd>\n" ++ insertMTableBody body ++ "</mtd>\n</mtr>\n</mtable>\n"
    --TODO Alignment parameters for array?
generateFromASTElem (InlineCommand name _ (firstElement:secondElement:_))
    | name == "frac" = "<mfrac>\n<mrow>\n" ++ generate' firstElement ++ "</mrow>\n<mrow>\n" ++ generate' secondElement ++ "</mrow>\n</mfrac>\n"
    | name == "cfrac" = "<mfrac>\n<mrow>\n" ++ generate' firstElement ++ "</mrow>\n<mrow>\n" ++ generate' secondElement ++ "</mrow>\n</mfrac>\n"
    | name == "binom" = "<mfenced>\n<mfrac linethickness=\"0\">\n<mrow>\n" ++ generate' firstElement ++ "</mrow>\n<mrow>\n" ++ generate' secondElement ++ "</mrow>\n</mfrac>\n</mfenced>\n"
generateFromASTElem (InlineCommand name params (firstElement:_))
    | name == "sqrt" = "<mroot>\n<mrow>\n" ++ generate' firstElement ++ "</mrow>\n<mrow>\n" ++ generate' params ++ "</mrow>\n</mroot>\n"
generateFromASTElem (InlineCommand name _ (firstElement:_))
    | name == "text" = "<mtext>\n<mrow>\n" ++ generate' firstElement ++ "</mrow>\n</mtext>\n"
    | name == "mathrm" = "<mtext>\n<mrow>\n" ++ generate' firstElement ++ "</mrow>\n</mtext>\n"
    | name `elem` accentNames = "<mover accent=\"true\">\n<mrow>\n" ++ generate' firstElement ++ "</mrow>\n"  ++ ((fromList accentTransList) ! name) ++ "\n</mover>\n"
    | otherwise = ""
generateFromASTElem (ASTSub body) = "<msub>\n<mi></mi>\n<mrow>\n" ++ generate' body ++ "</mrow>\n</msub>\n"
generateFromASTElem (ASTSup body) = "<msup>\n<mi></mi>\n<mrow>\n" ++ generate' body ++ "</mrow>\n</msup>\n"
generateFromASTElem (BodylessCommand commandName) = (translateSimpleCommandName commandName) ++ "\n"
generateFromASTElem (ASTOperator name)
    | name == "<" = "<mo>&lt;</mo>\n"
    | name == ">" = "<mo>&gt;</mo>\n"
    | name == "{" = "<mo>&lbrace;</mo>\n"
    | name == "}" = "<mo>&rbrace;</mo>\n"
    | name == "}" = "<mi>&amp;</mi>\n"
    | otherwise = "<mo>" ++ name ++ "</mo>\n"
generateFromASTElem (Variable value) = "<mi>" ++ [value] ++ "</mi>\n"
generateFromASTElem (MN value) = "<mn>" ++ value ++ "</mn>\n"
generateFromASTElem _ = ""

translateSimpleCommandName :: [Char] -> [Char]
translateSimpleCommandName name = (fromList (trigTransList ++ greekTransList ++ logicTransList ++ relationTransList ++ binaryTransList ++ delimiterTransList ++ escapedCharacterTransList ++ otherTransList)) ! name

insertMTableBody :: [ASTModel] -> [Char]
insertMTableBody [] = ""
insertMTableBody ((ASTOperator "&") : rest) = "</mtd>\n<mtd>" ++ insertMTableBody rest
insertMTableBody ((ASTOperator "\n") : rest) = "</mtd>\n</mtr>\n<mtr>\n<mtd>" ++ insertMTableBody rest
insertMTableBody (firstElement : rest) = generateFromASTElem firstElement ++ insertMTableBody rest

trigTransList :: [(String, String)]
trigTransList = [("sin","<mi>sin</mi>"),("arcsin","<mi>arcsin</mi>"),("sinh","<mi>sinh</mi>"),("sec","<mi>sec</mi>"),("cos","<mi>cos</mi>"),("arccos","<mi>arccos</mi>"),("cosh","<mi>cosh</mi>"),("csc","<mi>csc</mi>"),("tan","<mi>tan</mi>"),("arctan","<mi>arctan</mi>"),("tanh","<mi>tanh</mi>"),("cot","<mi>cot</mi>"),("coth","<mi>coth</mi>")]

greekTransList :: [(String, String)]
greekTransList = [("Alpha","<mi>&Alpha;</mi>"), ("alpha","<mi>&alpha;</mi>"), ("Beta","<mi>&Beta;</mi>"), ("beta","<mi>&beta;</mi>"), ("gamma","<mi>&gamma;</mi>"), ("Gamma","<mi>&Gamma;</mi>"), ("delta","<mi>&delta;</mi>"), ("Delta","<mi>&Delta;</mi>"),("Epsilon","<mi>&Epsilon;</mi>"), ("epsilon","<mi>&epsilon;</mi>"), ("varepsilon","<mi>&varepsilon;</mi>"), ("Zeta","<mi>&Zeta;</mi>"), ("zeta","<mi>&zeta;</mi>"), ("Eta","<mi>&Eta;</mi>"), ("eta","<mi>&eta;</mi>"), ("Theta","<mi>&Theat;</mi>"), ("theta","<mi>&theat;</mi>"), ("vartheta","<mi>&vartheta;</mi>"), ("Iota","<mi>&Iota;</mi>"), ("iota","<mi>&iota;</mi>"), ("Kappa","<mi>&Kappa;</mi>"), ("kappa","<mi>&kappa;</mi>"), ("varkappa","<mi>&varkappa;</mi>"), ("Lambda","<mi>&Lambda;</mi>"), ("lambda","<mi>&lambda;</mi>"), ("Mu","<mi>&Mu;</mi>"), ("mu","<mi>&mu;</mi>"), ("Nu","<mi>&Nu;</mi>"), ("nu","<mi>&nu;</mi>"), ("Xi","<mi>&Xi;</mi>"), ("xi","<mi>&xi;</mi>"), ("Pi","<mi>&Pi;</mi>"), ("pi","<mi>&pi;</mi>"), ("varpi","<mi>&varpi;</mi>"), ("Rho","<mi>&Rho;</mi>"), ("rho","<mi>&rho;</mi>"), ("varrho","<mi>&varrho;</mi>"), ("Sigma","<mi>&Sigma;</mi>"), ("sigma","<mi>&sigma;</mi>"), ("varsigma","<mi>&varsigma;</mi>"), ("Tau","<mi>&Tau;</mi>"), ("tau","<mi>&tau;</mi>"), ("Upsilon","<mi>&Upsilon;</mi>"), ("upsilon","<mi>&upsilon;</mi>"), ("Phi","<mi>&Phi;</mi>"), ("phi","<mi>&phi;</mi>"), ("varphi","<mi>&varphi;</mi>"), ("Chi","<mi>&Chi;</mi>"), ("chi","<mi>&chi;</mi>"), ("Psi","<mi>&Psi;</mi>"), ("psi","<mi>&psi;</mi>"), ("Omega","<mi>&Omega;</mi>"), ("omega","<mi>&omega;</mi>"), ("Omicron","<mi>&Omicron;</mi>"), ("omicron","<mi>&omicron;</mi>")]

logicTransList :: [(String, String)]
logicTransList = [("neg","<mi>&not;</mi>"), ("land","<mi>&and;</mi>"), ("lor","<mi>&or;</mi>"), ("forall","<mi>&forall;</mi>"), ("exists","<mi>&exists;</mi>"), ("nexists","<mi>&nexists;</mi>"), ("leftarrow","<mi>&larr;</mi>"), ("gets","<mi>&larr;</mi>"), ("rightarrow","<mi>&rarr;</mi>"), ("Rightarrow","<mi>&rArr;</mi>"), ("to","<mi>&rarr;</mi>"), ("leftrightarrow","<mi>&harr;</mi>"), ("Leftrightarrow","<mi>&hArr;</mi>"), ("mapsto","<mi>&mapsto;</mi>"), ("implies","<mi>&rArr;</mi>"), ("iff","<mi>&hArr;</mi>"), ("in","<mi>&isin;</mi>"), ("notin","<mi>&notin;</mi>"), ("ni","<mi>&ni;</mi>"), ("top","<mi>&top;</mi>"), ("bot","<mi>&bot;</mi>"), ("subset","<mi>&sub;</mi>"), ("supset","<mi>&sup;</mi>"), ("emptyset","<mi>&empty;</mi>"), ("varnothing","<mi>&empty;</mi>")]

relationTransList :: [(String, String)]
relationTransList = [("parallel","<mi>&spar;</mi>"), ("nparallel","<mi>&npar;</mi>"), ("leq","<mi>&le;</mi>"), ("geq","<mi>&ge;</mi>"), ("doteq","<mi>&doteq;</mi>"), ("asymp","<mi>&asympeq;</mi>"), ("bowtie","<mi>&bowtie;</mi>"), ("ll","<mi>&ll;</mi>"), ("gg","<mi>&g;</mi>"), ("equiv","<mi>&equiv;</mi>"), ("vdash","<mi>&vdash;</mi>"), ("dashv","<mi>&dashv;</mi>"), ("subset","<mi>&sub;</mi>"), ("supset","<mi>&sup;</mi>"), ("approx","<mi>&approx;</mi>"), ("in","<mi>&isin;</mi>"), ("ni","<mi>&ni;</mi>"), ("subseteq","<mi>&subseteq;</mi>"), ("supseteq","<mi>&supseteq;</mi>"), ("cong","<mi>&cong;</mi>"), ("smile","<mi>&smile;</mi>"), ("frown","<mi>&frown;</mi>"), ("nsubseteq","<mi>&nsubseteq;</mi>"), ("nsupseteq","<mi>&nsupseteq;</mi>"), ("simeq","<mi>&simeq;</mi>"), ("models","<mi>&models;</mi>"), ("notin","<mi>&notin;</mi>"), ("sqsubset","<mi>&sqsubset;</mi>"), ("sqsupset","<mi>&sqsupset;</mi>"), ("sim","<mi>&sim;</mi>"), ("perp","<mi>&perp;</mi>"), ("mid","<mi>&mid;</mi>"), ("sqsubseteq","<mi>&sqsubseteq;</mi>"), ("sqsupseteq","<mi>&sqsupseteq;</mi>"), ("propto","<mi>&propto;</mi>"), ("prec","<mi>&prec;</mi>"), ("succ","<mi>&succ;</mi>"), ("preceq","<mi>&preceq;</mi>"), ("succeq","<mi>&succeq;</mi>"), ("neq","<mi>&ne;</mi>"), ("sphericalangle","<mi>&angmsd;</mi>"), ("measuredangle","<mi>&angmsd;</mi>")]

binaryTransList :: [(String, String)]
binaryTransList = [("pm","<mi>&pm;</mi>"), ("cap","<mi>&cap;</mi>"), ("diamond","<mi>&diamond;</mi>"), ("oplus","<mi>&oplus;</mi>"), ("mp","<mi>&mp;</mi>"), ("cup","<mi>&cup;</mi>"), ("bigtriangleup","<mi>&bigtriangleup;</mi>"), ("ominus","<mi>&ominus;</mi>"), ("times","<mi>&times;</mi>"), ("uplus","<mi>&uplus;</mi>"), ("bigtriangledown","<mi>&bigtriangledown;</mi>"), ("otimes","<mi>&otimes;</mi>"), ("div","<mi>&div;</mi>"), ("sqcap","<mi>&sqcap;</mi>"), ("triangleleft","<mi>&triangleleft;</mi>"), ("oslash","<mi>&oslash;</mi>"), ("ast","<mi>&ast;</mi>"), ("sqcup","<mi>&sqcup;</mi>"), ("triangleright","<mi>&triangleright;</mi>"), ("odot","<mi>&odot;</mi>"), ("star","<mi>&starf;</mi>"), ("vee","<mi>&vee;</mi>"), ("bigcirc","<mi>&bigcirc;</mi>"), ("circ","<mi>&cir;</mi>"), ("dagger","<mi>&dagger;</mi>"), ("wedge","<mi>&wedge;</mi>"), ("bullet","<mi>&bullet;</mi>"), ("setminus","<mi>&setminus;</mi>"), ("ddagger","<mi>&ddagger;</mi>"), ("cdot","<mi>&centerdot;</mi>"), ("wr","<mi>&wr;</mi>"), ("amalg","<mi>&amalg;</mi>")]

delimiterTransList :: [(String, String)]
delimiterTransList = [("|","<mi>&Vert;</mi>"), ("backslash","<mi>&Backslash;</mi>"), ("{","<mi>&lbrace;</mi>"), ("}","<mi>&rbrace;</mi>"), ("langle","<mi>&langle;</mi>"), ("rangle","<mi>&rangle;</mi>"), ("uparrow","<mi>&uparrow;</mi>"), ("Uparrow","<mi>&Uparrow;</mi>"), ("lceil","<mi>&lceil;</mi>"), ("rceil","<mi>&rceil;</mi>"), ("downarrow","<mi>&downarrow;</mi>"), ("Downarrow","<mi>&Downarrow;</mi>"), ("lfloor","<mi>&lfloor;</mi>"), ("rfloor","<mi>&rfloor;</mi>")]

escapedCharacterTransList :: [(String, String)]
escapedCharacterTransList = [("\\","<mi>\\</mi>"), ("{","<mi>{</mi>"), ("}","<mi>}</mi>"), ("$","<mi>$</mi>"), ("^","<mi>^</mi>"), ("_","<mi>_</mi>"), ("%","<mi>%</mi>"), ("~","<mi>~</mi>"), ("#","<mi>#</mi>"), ("&","<mi>&amp;</mi>")]

otherTransList :: [(String, String)]
otherTransList = [("prod","<mi>&prod;</mi>"), ("sum","<mi>&sum;</mi>"), ("lim","<mi>lim</mi>"), ("int","<mi>&int;</mi>"), ("iint","<mi>&Int;</mi>"), ("iiint","<mi>&iiint;</mi>"), ("iiiint","<mi>&iiiint;</mi>"), ("exp","<mi>&exponentiale;</mi>"), ("partial","<mi>&part;</mi>"), ("imath","<mi>&imath;</mi>"), ("Re","<mi>&Re;</mi>"), ("nabla","<mi>&nabla;</mi>"), ("aleph","<mi>&aleph;</mi>"), ("eth","<mi>&eth;</mi>"), ("jmath","<mi>&jmath;</mi>"), ("Im","<mi>&Im;</mi>"), ("Box","<mi>&square;</mi>"), ("beth","<mi>&beth;</mi>"), ("hbar","<mi>&hbar;</mi>"), ("ell","<mi>&ell;</mi>"), ("wp","<mi>&wp;</mi>"), ("infty","<mi>&infin;</mi>"), ("gimel","<mi>&gimel;</mi>"), ("left(","<mi>(</mi>"),("right)","<mi>)</mi>"),("left[","<mi>[</mi>"),("right]","<mi>]</mi>"),("left|","<mi>|</mi>"),("right|","<mi>|</mi>"), ("doubleOr","<mi>&Vert;</mi>"), ("dots","<mi>&hellip;</mi>"), ("ddots","<mi>&dtdot;</mi>"), ("cdots","<mi>&ctdot;</mi>"), ("vdots","<mi>&vellip;</mi>"), ("ldots","<mi>&hellip;</mi>"), ("textbackslash","<mi>\\</mi>"), ("lbrace","<mi>{</mi>"), ("rbrace","<mi>}</mi>"), ("quad","<mi>&nbsp;</mi>"), ("hline","")]

productionNames :: [String]
productionNames = ["int","iint","iiint","iiiint","sum","prod","lim"]

accentNames :: [String]
accentNames = ["hat","grave","bar","acute","mathring","check","dot","vec","breve","tilde","ddot","widehat","widetilde"]

accentTransList :: [(String, String)]
accentTransList = [("hat","<mo>&and;</mo>"), ("grave","<mo>&grave;</mo>"), ("bar","<mo>-</mo>"), ("acute","<mo>&acute;</mo>"), ("mathring","<mo>&cir;</mo>"), ("check","<mo>&or;</mo>"), ("dot","<mo>&middot;</mo>"), ("vec","<mo>&rarr;</mo>"), ("breve","<mo>&breve;</mo>"), ("tilde","<mo>&Tilde;</mo>"), ("ddot","<mo>&DoubleDot;</mo>"), ("widehat","<mo>&Hat;</mo>"), ("widetilde","<mo>&Tilde;</mo>")]
