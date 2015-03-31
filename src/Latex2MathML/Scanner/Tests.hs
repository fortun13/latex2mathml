module Latex2MathML.Scanner.Tests (tests) where

import Test.HUnit
import Latex2MathML.Utils.Definitions
import Latex2MathML.Scanner.Main

tests :: Test
tests = TestList [TestLabel "testGr1" testGr1, TestLabel "testGr2" testGr2, TestLabel "testOth" testOth,
                  TestLabel "testTri" testTri, TestLabel "testSet" testSet, TestLabel "testDel" testDel,
                  TestLabel "testBin" testBin, TestLabel "testRel" testRel, TestLabel "test1" test1,
                  TestLabel "test2" test2, TestLabel "test3" test3, TestLabel "test4" test4,
                  TestLabel "test5" test5, TestLabel "test6" test6, TestLabel "test7" test7,
                  TestLabel "test8" test8, TestLabel "test9" test9, TestLabel "test10" test10,
                  TestLabel "test11" test11, TestLabel "test12" test12, TestLabel "test13" test13,
                  TestLabel "test14" test14, TestLabel "test15" test15, TestLabel "test16" test16,
                  TestLabel "test17" test17, TestLabel "test18" test18, TestLabel "test19" test19]

--generateTestList 0 = [(,)]
--generateTestList n = ("test"++n, )

testGr1 :: Test
testGr1 = TestCase (assertEqual "Greek symbols, part one"
    ([CommandBodyless "alpha", CommandBodyless "beta", CommandBodyless "Gamma", CommandBodyless "gamma", CommandBodyless "Delta",
    CommandBodyless "delta", CommandBodyless "epsilon", CommandBodyless "varepsilon", CommandBodyless "zeta", CommandBodyless "eta",
    CommandBodyless "Theta", CommandBodyless "theta", CommandBodyless "vartheta", CommandBodyless "iota", CommandBodyless "kappa",
    CommandBodyless "lambda", CommandBodyless "Lambda", CommandBodyless "mu"], "")
    (scan "\\alpha \\beta \\Gamma \\gamma \\Delta \\delta \\epsilon \\varepsilon \\zeta \\eta \\Theta \\theta \\vartheta \\iota \\kappa \\lambda \\Lambda \\mu"))

testGr2 :: Test
testGr2 = TestCase (assertEqual "Greek symbols, part two"
    ([CommandBodyless "nu", CommandBodyless "Xi", CommandBodyless "xi", CommandBodyless "Pi", CommandBodyless "pi",
     CommandBodyless "varpi", CommandBodyless "rho", CommandBodyless "varrho", CommandBodyless "Sigma", CommandBodyless "sigma",
     CommandBodyless "varsigma", CommandBodyless "tau", CommandBodyless "Upsilon", CommandBodyless "upsilon", CommandBodyless "Phi",
     CommandBodyless "phi", CommandBodyless "varphi", CommandBodyless "chi", CommandBodyless "Psi", CommandBodyless "psi",
     CommandBodyless "Omega", CommandBodyless "omega", CommandBodyless "omicron"], "")
    (scan "\\nu \\Xi \\xi \\Pi \\pi \\varpi \\rho \\varrho \\Sigma \\sigma \\varsigma \\tau \\Upsilon \\upsilon \\Phi \\phi \\varphi \\chi \\Psi \\psi \\Omega \\omega \\omicron"))

testOth :: Test
testOth = TestCase (assertEqual "Other symbols class"
    ([CommandBodyless "partial", CommandBodyless "imath", CommandBodyless "Re", CommandBodyless "nabla", CommandBodyless "aleph",
    CommandBodyless "eth", CommandBodyless "jmath", CommandBodyless "Im", CommandBodyless "Box", CommandBodyless "beth",
    CommandBodyless "hbar", CommandBodyless "ell", CommandBodyless "wp", CommandBodyless "infty", CommandBodyless "gimel"], "")
    (scan "\\partial \\imath \\Re \\nabla \\aleph \\eth \\jmath \\Im \\Box \\beth \\hbar \\ell \\wp \\infty \\gimel"))

testTri :: Test
testTri = TestCase (assertEqual "Trigonometric symbols"
    ([CommandBodyless "sin", CommandBodyless "arcsin", CommandBodyless "sinh", CommandBodyless "sec", CommandBodyless "cos",
    CommandBodyless "arccos", CommandBodyless "cosh", CommandBodyless "csc", CommandBodyless "tan", CommandBodyless "arctan",
    CommandBodyless "tanh", CommandBodyless "cot", CommandBodyless "arccot", CommandBodyless "coth"], "")
    (scan "\\sin \\arcsin \\sinh \\sec \\cos \\arccos \\cosh \\csc \\tan \\arctan \\tanh \\cot \\arccot \\coth"))

testSet :: Test
testSet = TestCase (assertEqual "Set and logic symbols"
    ([CommandBodyless "exists", CommandBodyless "nexists", CommandBodyless "forall", CommandBodyless "neg", CommandBodyless "subset",
    CommandBodyless "supset", CommandBodyless "in", CommandBodyless "notin", CommandBodyless "ni", CommandBodyless "lor",
    CommandBodyless "rightarrow", CommandBodyless "to", CommandBodyless "leftarrow", CommandBodyless "gets", CommandBodyless "mapsto",
    CommandBodyless "implies", CommandBodyless "Rightarrow", CommandBodyless "leftrightarrow", CommandBodyless "iff", CommandBodyless "Leftrightarrow",
    CommandBodyless "top", CommandBodyless "land", CommandBodyless "bot", CommandBodyless "emptyset", CommandBodyless "varnothing"], "")
    (scan "\\exists \\nexists \\forall \\neg \\subset \\supset \\in \\notin \\ni \\lor \\rightarrow \\to \\leftarrow \\gets \\mapsto \\implies \\Rightarrow \\leftrightarrow \\iff \\Leftrightarrow \\top \\land \\bot \\emptyset \\varnothing"))

testDel :: Test
testDel = TestCase (assertEqual "Delimiter symbols"
    ([CommandBodyless "uparrow", CommandBodyless "downarrow", CommandBodyless "|", Operator "}", CommandBodyless "Uparrow",
    CommandBodyless "Downarrow", Operator "/", CommandBodyless "langle", CommandBodyless "lceil", CommandBodyless "lfloor",
    CommandBodyless "backslash", CommandBodyless "rangle", CommandBodyless "rceil", CommandBodyless "rfloor"], "")
    (scan "\\uparrow \\downarrow \\| \\} \\Uparrow \\Downarrow / \\langle \\lceil \\lfloor \\backslash \\rangle \\rceil \\rfloor"))

testBin :: Test
testBin = TestCase (assertEqual "Binary symbols"
    ([CommandBodyless "pm", CommandBodyless "cap", CommandBodyless "diamond", CommandBodyless "oplus", CommandBodyless "mp",
    CommandBodyless "cup", CommandBodyless "ominus", CommandBodyless "times", CommandBodyless "uplus", CommandBodyless "bigtriangledown",
    CommandBodyless "bigtriangleup", CommandBodyless "otimes", CommandBodyless "div", CommandBodyless "sqcap", CommandBodyless "triangleleft",
    CommandBodyless "oslash", CommandBodyless "ast", CommandBodyless "sqcup", CommandBodyless "triangleright", CommandBodyless "odot",
    CommandBodyless "star", CommandBodyless "vee", CommandBodyless "bigcirc", CommandBodyless "circ", CommandBodyless "dagger",
    CommandBodyless "wedge", CommandBodyless "bullet", CommandBodyless "setminus", CommandBodyless "ddagger", CommandBodyless "cdot",
    CommandBodyless "wr", CommandBodyless "amalg"], "")
    (scan "\\pm \\cap \\diamond \\oplus \\mp \\cup \\ominus \\times \\uplus \\bigtriangledown \\bigtriangleup \\otimes \\div \\sqcap \\triangleleft \\oslash \\ast \\sqcup \\triangleright \\odot \\star \\vee \\bigcirc \\circ \\dagger \\wedge \\bullet \\setminus \\ddagger \\cdot \\wr \\amalg"))

testRel :: Test
testRel = TestCase (assertEqual "Relation symbols"
    ([CommandBodyless "parallel", CommandBodyless "nparallel", CommandBodyless "leq", CommandBodyless "geq", CommandBodyless "doteq",
    CommandBodyless "asymp", CommandBodyless "bowtie", CommandBodyless "ll", CommandBodyless "gg", CommandBodyless "equiv",
    CommandBodyless "vdash", CommandBodyless "dashv", CommandBodyless "subset", CommandBodyless "supset", CommandBodyless "approx",
    CommandBodyless "in", CommandBodyless "ni", CommandBodyless "subseteq", CommandBodyless "supseteq", CommandBodyless "cong",
    CommandBodyless "smile", CommandBodyless "frown", CommandBodyless "nsubseteq", CommandBodyless "nsupseteq", CommandBodyless "sqsubset",
    CommandBodyless "sqsupset", CommandBodyless "simeq", CommandBodyless "models", CommandBodyless "notin", CommandBodyless "sim",
    CommandBodyless "perp", CommandBodyless "mid", CommandBodyless "sqsubseteq", CommandBodyless "sqsupseteq", CommandBodyless "propto",
    CommandBodyless "prec", CommandBodyless "succ", CommandBodyless "preceq", CommandBodyless "succeq", CommandBodyless "neq"], "")
    (scan "\\parallel \\nparallel \\leq \\geq \\doteq \\asymp \\bowtie \\ll \\gg \\equiv \\vdash \\dashv \\subset \\supset \\approx \\in \\ni \\subseteq \\supseteq \\cong \\smile \\frown \\nsubseteq \\nsupseteq \\sqsubset \\sqsupset \\simeq \\models \\notin \\sim \\perp \\mid \\sqsubseteq \\sqsupseteq \\propto \\prec \\succ \\preceq \\succeq \\neq"))


test1 :: Test
test1 = TestCase (assertEqual "Testing simple Sup; expression: k^2"
    ([MyStr "k",Sup [MyNum "2"]],"")
    (scan "k^2"))

test2 :: Test
test2 = TestCase (assertEqual "Nested commands; expression: \\frac{1 \\frac{ \\frac{2 + 3 - 4}{3}}{4}}{5}"
    ([InlineCommand "frac" [] [[MyNum "1",InlineCommand "frac" [] [[InlineCommand "frac" [] [[MyNum "2",Operator "+",MyNum "3",Operator "-",MyNum "4"],[MyNum "3"]]],[MyNum "4"]]],[MyNum "5"]]],"")
    (scan "\\frac{1 \\frac{ \\frac{2 + 3 - 4}{3}}{4}}{5}"))

test3 :: Test
test3 = TestCase (assertEqual "Operators and Brackets; expression: =+*/!<>|:() ' '' ''' ''''"
    ([Operator "=",Operator "+",Operator "*",Operator "/",Operator "!",Operator "<",Operator ">",Operator "|",Operator ":",Operator "(",Operator ")",Operator "'",Operator "''",Operator "'''",Operator"''''"],"")
    (scan "=+*/!<>|:() ' '' ''' ''''"))

test4 :: Test
test4 = TestCase (assertEqual "Sup with command; expression: x^\\frac{1}{2}"
    ([MyStr "x",Sup [InlineCommand "frac" [] [[MyNum "1"],[MyNum "2"]]]],"")
    (scan "x^\\frac{1}{2}"))

test5 :: Test
test5 = TestCase (assertEqual "Sup and Sub test; expression: k_{n+1} = n^2 + k_n^2 - k_{n-1}"
    ([MyStr "k",Sub [MyStr "n",Operator "+",MyNum "1"],Operator "=",MyStr "n",Sup [MyNum "2"],Operator "+",MyStr "k",Sub [MyStr "n"],Sup [MyNum "2"],Operator "-",MyStr "k",Sub [MyStr "n",Operator "-",MyNum "1"]],"")
    (scan "k_{n+1} = n^2 + k_n^2 - k_{n-1}"))

test6 :: Test
test6 = TestCase (assertEqual "Inconvinient comment; expression: \\frac{2}%stupid place for comment \n{3}"
    ([InlineCommand "frac" [] [[MyNum "2"],[MyNum "3"]]],"")
    (scan "\\frac{2}%stupid place for comment \n{3}"))

test7 :: Test
test7 = TestCase (assertEqual "Square bracket testing; expression: [] \\sqrt[n]{1}{2}"
    ([Operator "[",Operator "]", InlineCommand "sqrt" [MyStr "n"] [[MyNum "1"],[MyNum "2"]]],"")
    (scan "[] \\sqrt[n]{1}{2}"))

test8 :: Test
test8 = TestCase (assertEqual "Complex Command testing; expression: \\begin{matrix} \\alpha & 2 \\\\ 3 & 4 \\end{matrix}"
    ([ComplexCommand "matrix" [] [CommandBodyless "alpha",Operator "&",MyNum "2",Operator "\n",MyNum "3",Operator "&",MyNum "4"]],"")
    (scan "\\begin{matrix} \\alpha & 2 \\\\ 3 & 4 \\end{matrix}"))

test9 :: Test
test9 = TestCase (assertEqual "Number string with unncessary spaces: 12             13 "
    ([MyNum "1213"],"")
    (scan "12             13"))

test10 :: Test
test10 = TestCase (assertEqual "Number string with single space: 12 \\ 13"
        ([MyNum "12",Operator "s",MyNum "13"],"")
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

test15 :: Test
test15 = TestCase (assertEqual "wikipedia example: \\forall x \\in X, \\quad \n \\exists y \\leq \\epsilon"
    ([CommandBodyless "forall",MyStr "x",CommandBodyless "in",CommandBodyless "X",Operator ",",CommandBodyless "quad",CommandBodyless "exists",MyStr "y",CommandBodyless "leq",CommandBodyless "epsilon"],"")
    (scan "\\forall x \\in X, \\quad \n \\exists y \\leq \\epsilon"))

test16 :: Test
test16 = TestCase (assertEqual "wikipedia example (greek letters): \\alpha, \\Alpha, \\beta, \\Beta, \\gamma, \\Gamma, \\pi, \\Pi, \\phi, \\varphi, \\Phi"
    ([CommandBodyless "alpha",Operator ",",CommandBodyless "Alpha",Operator ",",CommandBodyless "beta",Operator ",",CommandBodyless "Beta",Operator ",",CommandBodyless "gamma",Operator ",",CommandBodyless "Gamma",Operator ",",CommandBodyless "pi",Operator ",",CommandBodyless "Pi",Operator ",",CommandBodyless "phi",Operator ",",CommandBodyless "varphi",Operator ",",CommandBodyless "Phi"],"")
    (scan "\\alpha, \\Alpha, \\beta, \\Beta, \\gamma, \\Gamma, \\pi, \\Pi, \\phi, \\varphi, \\Phi"))

test17 :: Test
test17 = TestCase (assertEqual "wikipedia example (operators 1): \\cos (2\\theta) = \\cos^2 \\theta - \\sin^2 \\theta"
    ([CommandBodyless "cos",Operator "(",MyNum "2",CommandBodyless "theta",Operator ")",Operator "=",CommandBodyless "cos",Sup [MyNum "2"],CommandBodyless "theta",Operator "-",CommandBodyless "sin",Sup [MyNum "2"],CommandBodyless "theta"],"")
    (scan "\\cos (2\\theta) = \\cos^2 \\theta - \\sin^2 \\theta"))

test18 :: Test
test18 = TestCase (assertEqual "wikipedia example (operators 2): \\lim_{x \\to \\infty} \\exp(-x) = 0"
    ([CommandBodyless "lim",Sub [MyStr "x", CommandBodyless "to", CommandBodyless "infty"],CommandBodyless "exp", Operator "(",Operator "-",MyStr "x",Operator ")",Operator "=",MyNum "0"],"")
    (scan "\\lim_{x \\to \\infty} \\exp(-x) = 0"))

test19 :: Test
test19 = TestCase (assertEqual "wikipedia example: f(n) = n^5 + 4n^2 + 2 |_{n=17}"
    ([MyStr "f",Operator "(",MyStr "n",Operator ")",Operator "=",MyStr "n",Sup [MyNum "5"],Operator "+",MyNum "4",MyStr "n",Sup [MyNum "2"],Operator "+",MyNum "2",Operator "|",Sub [MyStr "n",Operator "=",MyNum "17"]],"")
    (scan "f(n) = n^5 + 4n^2 + 2 |_{n=17}"))
