module Latex2MathML.Utils.Definitions where

import Data.Set (fromList,Set)
import Data.Map (fromList,Map)

data Token = Command String |
    MyStr String |
    Sub |
    Sup |
    Operator String |
    MyNum String |
    End |
    BodyBegin |
    BodyEnd |
    Error
    deriving (Show,Eq)

data ASTModel = BodylessCommand String |
   InlineCommand String [ASTModel] [[ASTModel]] |
   ComplexCommand String [ASTModel] [ASTModel] |
   ASTOperator String |
   ASTSub [ASTModel] |
   ASTSup [ASTModel] |
   Variable Char |
   MN String |
   Empty |
   ComplexEnd
   deriving (Show,Eq)

operators :: String
operators = "+-*/=!():<>|[]&\n,.'$@"

commands :: Set String
commands = Data.Set.fromList (accentList ++ greekList ++ relationList ++ binaryList ++ logicList ++ delimiterList ++ trigList ++ inlinesList ++ escapedCharacterList ++ otherList)

accentList :: [String]
accentList = ["hat","grave","bar","acute","mathring","check","dot","vec","breve","tilde","ddot","widehat","widetilde"]

greekList :: [String]
greekList = ["Alpha","alpha","Beta","beta","Gamma","gamma","Delta","delta","Epsilon","epsilon","varepsilon","Zeta","zeta","Eta","eta","Theta","theta","vartheta","Iota","iota","Kappa","kappa","varkappa","Lambda","lambda","Mu","mu","Nu","nu","Xi","xi","Pi","pi","varpi","Rho","rho","varrho","Sigma","sigma","varsigma","Tau","tau","Upsilon","upsilon","Phi","phi","varphi","Chi","chi","Psi","psi","Omega","omega","Omicron","omicron"]

relationList :: [String]
relationList = ["parallel","nparallel","leq","geq","doteq","asymp","bowtie","ll","gg","equiv","vdash","dashv","subset","supset","approx","in","ni","subseteq","supseteq","cong","smile","frown","nsubseteq","nsupseteq","simeq","models","notin","sqsubset","sqsupset","sim","perp","mid","sqsubseteq","sqsupseteq","propto","prec","succ","preceq","succeq","neq","sphericalangle","measuredangle"]

binaryList :: [String]
binaryList = ["pm","cap","diamond","oplus","mp","cup","bigtriangleup","ominus","times","uplus","bigtriangledown","otimes","div","sqcap","triangleleft","oslash","ast","sqcup","triangleright","odot","star","vee","bigcirc","circ","dagger","wedge","bullet","setminus","ddagger","cdot","wr","amalg"]

logicList :: [String]
logicList = ["neg","land","lor","forall","exists","nexists","leftarrow","gets","rightarrow","Rightarrow","to","leftrightarrow","Leftrightarrow","mapsto","implies","iff","in","notin","ni","top","bot","subset","supset","emptyset","varnothing"]

delimiterList :: [String]
delimiterList = ["backslash","langle","rangle","uparrow","Uparrow","lceil","rceil","downarrow","Downarrow","lfloor","rfloor","{","}","(",")","[","]","|"]

escapedCharacterList :: [String]
escapedCharacterList = ["\\", "{", "}", "$", "^", "_", "%", "~", "#", "&"]

trigList :: [String]
trigList = ["sin","arcsin","sinh","sec","cos","arccos","cosh","csc","tan","arctan","tanh","cot","coth"]

otherList :: [String]
otherList = ["prod","sum","lim","int","iint","iiint","iiiint","exp","partial","imath","Re","nabla","aleph","eth","jmath","Im","Box","beth","hbar","ell","wp","infty","gimel","left(","right)","left[","right]","left|","right|", "doubleOr", "hline","dots","ddots","cdots","vdots","ldots","textbackslash", "lbrace", "rbrace", "quad"]

inlinesList :: [String]
inlinesList = ["frac","cfrac","sqrt","binom","mathrm","text"]

complex :: Set String
complex = Data.Set.fromList ["pmatrix","bmatrix","Bmatrix","vmatrix","Vmatrix","matrix","array"]

commandsArity :: Map String Int
commandsArity = Data.Map.fromList [("frac",2),("sqrt",1),("cfrac",2),("binom",2)]