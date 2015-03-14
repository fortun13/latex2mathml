module Scanner.Tests (tests) where

import Test.HUnit
import Scanner.Definitions
import Scanner.Main

tests :: Test
tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2, TestLabel "test3" test3, TestLabel "test4" test4, TestLabel "test5" test5, TestLabel "test6" test6]

test1 :: Test
test1 = TestCase (assertEqual "Testing simple Sup"
    ([MyStr "k",Sup [MyStr "2"]],"")
    (scan "k^2"))

test2 :: Test
test2 = TestCase (assertEqual "Nested commands"
    ([InlineCommand "frac" [] [[MyStr "1",InlineCommand "frac" [] [[InlineCommand "frac" [] [[MyStr "2",MyStr "+",MyStr "3",MyStr "-",MyStr "4"],[MyStr "3"]]],[MyStr "4"]]],[MyStr "5"]]],"")
    (scan "\\frac{1 \\frac{ \\frac{2 + 3 - 4}{3}}{4}}{5}"))

test3 :: Test
test3 = TestCase (assertEqual "Operators and Brackets"
    ([MyStr "=+*/!<>|:()"],"")
    (scan "=+*/!<>|:()"))

test4 :: Test
test4 = TestCase (assertEqual "Sup with command"
    ([MyStr "x",Sup [InlineCommand "frac" [] [[MyStr "1"],[MyStr "2"]]]],"")
    (scan "x^\\frac{1}{2}"))

test5 :: Test
test5 = TestCase (assertEqual "Sup and Sub test"
    ([MyStr "k",Sub [MyStr "n+1"],MyStr "=",MyStr "n",Sup [MyStr "2"],MyStr "+",MyStr "k",Sub [MyStr "n"],Sup [MyStr "2"],MyStr "-",MyStr "k",Sub [MyStr "n-1"]],"")
    (scan "k_{n+1} = n^2 + k_n^2 - k_{n-1}"))

test6 :: Test
test6 = TestCase (assertEqual "Inconvinient comment"
    ([InlineCommand "frac" [] [[MyNum "2"],[MyNum "3"]]],"")
    (scan "\\frac{2}%stupid place for comment \n{3}"))
