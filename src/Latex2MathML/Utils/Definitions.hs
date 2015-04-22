module Latex2MathML.Utils.Definitions where

-- in ComplexCommand and InlineCommand first [Token] is for parameters, second for actual command body
--
--data Token = CommandBodyless String |
--    InlineCommand String [Token] [[Token]] |
--    ComplexCommand String [Token] [Token] |
--    ArrayCommand String [Token] [Token] [Token] |
--    MyStr String |
--    Sub [Token] |
--    Sup [Token] |
--    Operator String |
--    MyNum String |
--    End |
--    ComplexEnd |
--    Error
--    deriving (Show,Eq)


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
   InlineCommand String [Token] [[Token]] |
   ComplexCommand String [Token] [Token] |
   ASTOperator String |
   ASTSub [Token] |
   ASTSup [Token] |
   Variable Char |
   MN String