module Scanner.Tests (tests) where

import Test.HUnit
import Scanner.Definitions
import Scanner.Main

tests :: Test
tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2, TestLabel "test3" test3, TestLabel "test4" test4, TestLabel "test5" test5, TestLabel "test6" test6, TestLabel "test7" test7, TestLabel "test8" test8]

test1 :: Test
test1 = TestCase (assertEqual "Testing simple Sup"
    ([MyStr "k",Sup [MyNum "2"]],"")
    (scan "k^2"))

test2 :: Test
test2 = TestCase (assertEqual "Nested commands"
    ([InlineCommand "frac" [] [[MyNum "1",InlineCommand "frac" [] [[InlineCommand "frac" [] [[MyNum "2",Operator '+',MyNum "3",Operator '-',MyNum "4"],[MyNum "3"]]],[MyNum "4"]]],[MyNum "5"]]],"")
    (scan "\\frac{1 \\frac{ \\frac{2 + 3 - 4}{3}}{4}}{5}"))

test3 :: Test
test3 = TestCase (assertEqual "Operators and Brackets"
    ([Operator '=',Operator '+',Operator '*',Operator '/',Operator '!',Operator '<',Operator '>',Operator '|',Operator ':',Operator '(',Operator ')'],"")
    (scan "=+*/!<>|:()"))

test4 :: Test
test4 = TestCase (assertEqual "Sup with command"
    ([MyStr "x",Sup [InlineCommand "frac" [] [[MyNum "1"],[MyNum "2"]]]],"")
    (scan "x^\\frac{1}{2}"))

test5 :: Test
test5 = TestCase (assertEqual "Sup and Sub test"
    ([MyStr "k",Sub [MyStr "n",Operator '+',MyNum "1"],Operator '=',MyStr "n",Sup [MyNum "2"],Operator '+',MyStr "k",Sub [MyStr "n"],Sup [MyNum "2"],Operator '-',MyStr "k",Sub [MyStr "n",Operator '-',MyNum "1"]],"")
    (scan "k_{n+1} = n^2 + k_n^2 - k_{n-1}"))

test6 :: Test
test6 = TestCase (assertEqual "Inconvinient comment"
    ([InlineCommand "frac" [] [[MyNum "2"],[MyNum "3"]]],"")
    (scan "\\frac{2}%stupid place for comment \n{3}"))

test7 :: Test
test7 = TestCase (assertEqual "Square bracket testing"
    ([Operator '[',Operator ']', InlineCommand "sqrt" [MyStr "n"] [[MyNum "1"],[MyNum "2"]]],"")
    (scan "[] \\sqrt[n]{1}{2}"))

test8 :: Test
test8 = TestCase (assertEqual "Complex Command testing"
    ([ComplexCommand "matrix" [] [MyNum "2",Operator '&',MyNum "2",Operator '\n',MyNum "3",Operator '&',MyNum "4"]],"")
    (scan "\\begin{matrix} 2 & 2 \\\\ 3 & 4 \\end{matrix}"))
