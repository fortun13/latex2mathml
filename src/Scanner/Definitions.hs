module Scanner.Definitions where

-- in ComplexCommand and InlineCommand first [Token] is for parameters, second for actual command body

data Token = CommandBodyless Bodylesstype |
    InlineCommand String [Token] [[Token]] |
    ComplexCommand String [Token] [Token] |
    MyStr String |
    Sub [Token] |
    Sup [Token] |
    Operator Char |
    MyNum String |
    End |
    ComplexEnd
    deriving (Show,Eq)

data Bodylesstype = Greek GreekSymbol Bool | Math | Relation | Binary | SetAndLogic deriving (Show,Eq)

--bool is fo "isUpper"
data GreekSymbol = Alpha | Beta | Gamma | Delta deriving (Show,Eq)

--instance Eq Token where
--    (CommandInline str1 atoms1) == (CommandInline str2 atoms2) = ((atoms1 == atoms2) && (str1 == str2))
--    (Operator op1) == (Operator op2) = op1 == op2
--    (MyNum num1) == (MyNum num2) = num1 == num2
--    (MyStr str1) == (MyStr str2) = str1 == str2
--    (Sub atoms1) == (Sub atoms2) = atoms1 == atoms2
--    (Sup atoms1) == (Sup atoms2) = atoms1 == atoms2
--    (Bracket br1) == (Bracket br2) = br1 == br2
--    End == End = True
