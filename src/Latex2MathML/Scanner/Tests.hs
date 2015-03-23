module Latex2MathML.Scanner.Tests (tests) where

import Test.HUnit
import Latex2MathML.Utils.Definitions
import Latex2MathML.Scanner.Main

tests :: Test
tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2, TestLabel "test3" test3, TestLabel "test4" test4, TestLabel "test5" test5, TestLabel "test6" test6, TestLabel "test7" test7, TestLabel "test8" test8, TestLabel "test9" test9, TestLabel "test10" test10, TestLabel "test11" test11, TestLabel "test12" test12, TestLabel "test13" test13, TestLabel "test14" test14]

test1 :: Test
test1 = TestCase (assertEqual "Testing simple Sup; expression: k^2"
    ([MyStr "k",Sup [MyNum "2"]],"")
    (scan "k^2"))

test2 :: Test
test2 = TestCase (assertEqual "Nested commands; expression: \\frac{1 \\frac{ \\frac{2 + 3 - 4}{3}}{4}}{5}"
    ([InlineCommand "frac" [] [[MyNum "1",InlineCommand "frac" [] [[InlineCommand "frac" [] [[MyNum "2",Operator '+',MyNum "3",Operator '-',MyNum "4"],[MyNum "3"]]],[MyNum "4"]]],[MyNum "5"]]],"")
    (scan "\\frac{1 \\frac{ \\frac{2 + 3 - 4}{3}}{4}}{5}"))

test3 :: Test
test3 = TestCase (assertEqual "Operators and Brackets; expression: =+*/!<>|:()"
    ([Operator '=',Operator '+',Operator '*',Operator '/',Operator '!',Operator '<',Operator '>',Operator '|',Operator ':',Operator '(',Operator ')'],"")
    (scan "=+*/!<>|:()"))

test4 :: Test
test4 = TestCase (assertEqual "Sup with command; expression: x^\\frac{1}{2}"
    ([MyStr "x",Sup [InlineCommand "frac" [] [[MyNum "1"],[MyNum "2"]]]],"")
    (scan "x^\\frac{1}{2}"))

test5 :: Test
test5 = TestCase (assertEqual "Sup and Sub test; expression: k_{n+1} = n^2 + k_n^2 - k_{n-1}"
    ([MyStr "k",Sub [MyStr "n",Operator '+',MyNum "1"],Operator '=',MyStr "n",Sup [MyNum "2"],Operator '+',MyStr "k",Sub [MyStr "n"],Sup [MyNum "2"],Operator '-',MyStr "k",Sub [MyStr "n",Operator '-',MyNum "1"]],"")
    (scan "k_{n+1} = n^2 + k_n^2 - k_{n-1}"))

test6 :: Test
test6 = TestCase (assertEqual "Inconvinient comment; expression: \\frac{2}%stupid place for comment \n{3}"
    ([InlineCommand "frac" [] [[MyNum "2"],[MyNum "3"]]],"")
    (scan "\\frac{2}%stupid place for comment \n{3}"))

test7 :: Test
test7 = TestCase (assertEqual "Square bracket testing; expression: [] \\sqrt[n]{1}{2}"
    ([Operator '[',Operator ']', InlineCommand "sqrt" [MyStr "n"] [[MyNum "1"],[MyNum "2"]]],"")
    (scan "[] \\sqrt[n]{1}{2}"))

test8 :: Test
test8 = TestCase (assertEqual "Complex Command testing; expression: \\begin{matrix} \\alpha & 2 \\\\ 3 & 4 \\end{matrix}"
    ([ComplexCommand "matrix" [] [CommandBodyless "alpha",Operator '&',MyNum "2",Operator '\n',MyNum "3",Operator '&',MyNum "4"]],"")
    (scan "\\begin{matrix} \\alpha & 2 \\\\ 3 & 4 \\end{matrix}"))

test9 :: Test
test9 = TestCase (assertEqual "Number string with unncessary spaces: 12             13 "
    ([MyNum "1213"],"")
    (scan "12             13"))

test10 :: Test
test10 = TestCase (assertEqual "Number string with single space: 12 \\ 13"
        ([MyNum "12",Operator 's',MyNum "13"],"")
        (scan "12 \\ 13"))

test11 :: Test
test11 = TestCase (assertEqual "Fraction without brackets 1: \\frac 1 2"
        ([InlineCommand "frac" [] [[MyNum "1"],[MyNum "2"]]],"")
        (scan "\\frac 1 2"))

test12 :: Test
test12 = TestCase (assertEqual "Fraction without brackets 2: \\frac12"
        ([InlineCommand "frac" [] [[MyNum "1"],[MyNum "2"]]],"")
        (scan "\\frac12"))

test13 :: Test
test13 = TestCase (assertEqual "Sum symbol, without arguments: \\sum{12}{34}"
        ([CommandBodyless "sum",MyNum "12",MyNum "34"],"")
        (scan "\\sum{12}{34}"))

test14 :: Test
test14 = TestCase (assertEqual "Sum symbol, with arguments: \\sum_{12}^{34}"
        ([CommandBodyless "sum",Sub [MyNum "12"],Sup [MyNum "34"]],"")
        (scan "\\sum_{12}^{34}"))
