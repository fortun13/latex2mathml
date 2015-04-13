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
                  TestLabel "test17" test17, TestLabel "test18" test18, TestLabel "test19" test19,
                  TestLabel "test20" test20, TestLabel "test21" test21, TestLabel "test22" test22,
                  TestLabel "test23" test23, TestLabel "test24" test24, TestLabel "test25" test25,
                  TestLabel "test26" test26, TestLabel "test27" test27, TestLabel "test28" test28,
                  TestLabel "test29" test29, TestLabel "test30" test30, TestLabel "testMath" testMath,
                  TestLabel "testAcc" testAcc]

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


testMath :: Test
testMath = TestCase (assertEqual "Test for Math symbols"
    ([CommandBodyless "prod",CommandBodyless "sum",CommandBodyless "lim",CommandBodyless "int"],"")
    (scan "\\prod \\sum \\lim \\int "))

testAcc :: Test
testAcc = TestCase (assertEqual "Test for Accent symbols"
    ([CommandBodyless "hat",CommandBodyless "grave",CommandBodyless "bar",CommandBodyless "acute",CommandBodyless "mathring",CommandBodyless "check",CommandBodyless "dot",CommandBodyless "vec",CommandBodyless "breve",CommandBodyless "tilde",CommandBodyless "ddot",CommandBodyless "widehat",CommandBodyless "widetilde"],"")
    (scan "\\hat \\grave \\bar \\acute \\mathring \\check \\dot \\vec \\breve \\tilde \\ddot \\widehat \\widetilde"))


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
        ([InlineCommand "sum" [] [[MyNum "12"],[MyNum "34"]]],"")
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

test20 :: Test
test20 = TestCase (assertEqual "example: \frac{n!}{k!(n-k)!} = \binom{n}{k}"
    ([InlineCommand "frac" [] [[MyStr "n",Operator "!"],[MyStr "k",Operator "!",Operator "(",MyStr "n",Operator "-",MyStr "k",Operator ")",Operator "!"]],Operator "=",InlineCommand "binom" [] [[MyStr "n"],[MyStr "k"]]],"")
    (scan "\\frac{n!}{k!(n-k)!} = \\binom{n}{k}"))

test21 :: Test
test21 = TestCase (assertEqual "example: \\frac{\\frac{1}{x}+\\frac{1}{y}}{y-z}"
    ([InlineCommand "frac" [] [[InlineCommand "frac" [] [[MyNum "1"],[MyStr "x"]],Operator "+",InlineCommand "frac" [] [[MyNum "1"],[MyStr "y"]]],[MyStr "y",Operator "-",MyStr "z"]]],"")
    (scan "\\frac{\\frac{1}{x}+\\frac{1}{y}}{y-z}"))

test22 :: Test
test22 = TestCase (assertEqual "example: x = a_0 + \\cfrac{1}{a_1 + \\cfrac{1}{a_2 + \\cfrac{1}{a_3 + \\cfrac{1}{a_4}}}}"
    ([MyStr "x",Operator "=",MyStr "a",Sub [MyNum "0"],Operator "+",InlineCommand "cfrac" [] [[MyNum "1"],[MyStr "a",Sub [MyNum "1"],Operator "+",InlineCommand "cfrac" [] [[MyNum "1"],[MyStr "a",Sub [MyNum "2"],Operator "+",InlineCommand "cfrac" [] [[MyNum "1"],[MyStr "a",Sub [MyNum "3"],Operator "+",InlineCommand "cfrac" [] [[MyNum "1"],[MyStr "a",Sub [MyNum "4"]]]]]]]]]],"")
    (scan "x = a_0 + \\cfrac{1}{a_1 + \\cfrac{1}{a_2 + \\cfrac{1}{a_3 + \\cfrac{1}{a_4}}}}"))

test23 :: Test
test23 = TestCase (assertEqual "example: \\frac{\\begin{array}[b]{r}\\left(x_1 x_2 \\right) \\\\ \\times \\left( x'_1 x'_2 \\right) \\end{array}}{\\left( y_1y_2y_3y_4 \\right)}"
    ([InlineCommand "frac" [] [[ComplexCommand "array" [MyStr "b", MyStr "r"] [CommandBodyless "left",Operator "(",MyStr "x",Sub [MyNum "1"],MyStr "x",Sub [MyNum "2"],CommandBodyless "right",Operator ")",Operator "s",CommandBodyless "times",CommandBodyless "left",Operator "(",MyStr "x",Operator "'",Sub [MyNum "1"],MyStr "x",Operator "'",Sub [MyNum "2"],CommandBodyless "right",Operator ")"]],[CommandBodyless "left",Operator "(",MyStr "y",Sub [MyNum "1"],MyStr "y",Sub [MyNum "2"],MyStr "y",Sub [MyNum "3"],MyStr "y",Sub [MyNum "4"],CommandBodyless "right",Operator ")"]]],"")
    (scan "\\frac{\\begin{array}[b]{r}\\left(x_1 x_2 \\right) \\ \\times \\left( x'_1 x'_2 \\right) \\end{array}}{\\left( y_1y_2y_3y_4 \\right)}"))

test24 :: Test
test24 = TestCase (assertEqual "example: \\sqrt[n]{1+x+x^2+x^3+\\ldots}"
    ([InlineCommand "sqrt" [MyStr "n"] [[MyNum "1",Operator "+",MyStr "x",Operator "+",MyStr "x",Sup [MyNum "2"],Operator "+",MyStr "x",Sup [MyNum "3"],Operator "+",CommandBodyless "ldots"]]],"")
    (scan "\\sqrt[n]{1+x+x^2+x^3+\\ldots}"))

test25 :: Test
test25 = TestCase (assertEqual "example: \\int_0^\\infty \\mathrm{e}^{-x} \\mathrm{d}x"
    ([CommandBodyless "int",Sub [MyNum "0"],Sup [CommandBodyless "infty"],InlineCommand "mathrm" [] [[MyStr "e"]],Sup [Operator "-",MyStr "x"],InlineCommand "mathrm" [] [[MyStr "d"]],MyStr "x"],"")
    (scan "\\int_0^\\infty \\mathrm{e}^{-x} \\mathrm{d}x"))

test26 :: Test
test26 = TestCase (assertEqual "example: \\begin{matrix} a & b & c \\\\ d & e & f \\\\ g & h & i \\end{matrix}"
    ([ComplexCommand "matrix" [] [MyStr "a",Operator "&",MyStr "b",Operator "&",MyStr "c",Operator "\n",MyStr "d",Operator "&",MyStr "e",Operator "&",MyStr "f",Operator "\n",MyStr "g",Operator "&",MyStr "h",Operator "&",MyStr "i"]],"")
    (scan "\\begin{matrix} a & b & c \\\\ d & e & f \\\\ g & h & i \\end{matrix}"))

test27 :: Test
test27 = TestCase (assertEqual "example: \\begin{matrix} -1 & 3 \\\\ 2 & -4 \\end{matrix} = \\begin{matrix}[r] -1 & 3 \\\\ 2 & -4 \\end{matrix}"
    ([ComplexCommand "matrix" [] [Operator "-",MyNum "1",Operator "&",MyNum "3",Operator "\n",MyNum "2",Operator "&",Operator "-",MyNum "4"],Operator "=",ComplexCommand "matrix" [MyStr "r"] [Operator "-",MyNum "1",Operator "&",MyNum "3",Operator "\n",MyNum "2",Operator "&",Operator "-",MyNum "4"]],"")
    (scan "\\begin{matrix} -1 & 3 \\\\ 2 & -4 \\end{matrix} = \\begin{matrix}[r] -1 & 3 \\\\ 2 & -4 \\end{matrix}"))

test28 :: Test
test28 = TestCase (assertEqual "example: A_{m,n} = \\begin{matrix} a_{1,1} & a_{1,2} & \\cdots & a_{1,n} \\\\ a_{2,1} & a_{2,2} & \\cdots & a_{2,n} \\\\ \\vdots  & \\vdots  & \\ddots & \\vdots  \\\\ a_{m,1} & a_{m,2} \\cdots & a_{m,n} \\end{matrix}"
    ([CommandBodyless "A",Sub [MyStr "m",Operator ",",MyStr "n"],Operator "=",ComplexCommand "matrix" [] [MyStr "a",Sub [MyNum "1",Operator ",",MyNum "1"],Operator "&",MyStr "a",Sub [MyNum "1",Operator ",",MyNum "2"],Operator "&",CommandBodyless "cdots",Operator "&",MyStr "a",Sub [MyNum "1",Operator ",",MyStr "n"],Operator "\n",MyStr "a",Sub [MyNum "2",Operator ",",MyNum "1"],Operator "&",MyStr "a",Sub [MyNum "2",Operator ",",MyNum "2"],Operator "&",CommandBodyless "cdots",Operator "&",MyStr "a",Sub [MyNum "2",Operator ",",MyStr "n"],Operator "\n",CommandBodyless "vdots",Operator "&",CommandBodyless "vdots",Operator "&",CommandBodyless "ddots",Operator "&",CommandBodyless "vdots",Operator "\n",MyStr "a",Sub [MyStr "m",Operator ",",MyNum "1"],Operator "&",MyStr "a",Sub [MyStr "m",Operator ",",MyNum "2"],CommandBodyless "cdots",Operator "&",MyStr "a",Sub [MyStr "m",Operator ",",MyStr "n"]]],"")
    (scan "A_{m,n} = \\begin{matrix} a_{1,1} & a_{1,2} & \\cdots & a_{1,n} \\\\ a_{2,1} & a_{2,2} & \\cdots & a_{2,n} \\\\ \\vdots  & \\vdots  & \\ddots & \\vdots  \\\\ a_{m,1} & a_{m,2} \\cdots & a_{m,n} \\end{matrix}"))

test29 :: Test
test29 = TestCase (assertEqual "M = \\begin{matrix} \\frac{5}{6} & \\frac{1}{6} & 0 \\\\ [0.3em] \\frac{5}{6} & 0 & \\frac{1}{6} \\\\[0.3em] 0 & \\frac{5}{6} & \\frac{1}{6} \\end{matrix}"
    ([CommandBodyless "M",Operator "=",ComplexCommand "matrix" [] [InlineCommand "frac" [] [[MyNum "5"],[MyNum "6"]],Operator "&",InlineCommand "frac" [] [[MyNum "1"],[MyNum "6"]],Operator "&",MyNum "0",Operator "\n",Operator "[",MyNum "0.3",MyStr "em",Operator "]",InlineCommand "frac" [] [[MyNum "5"],[MyNum "6"]],Operator "&",MyNum "0",Operator "&",InlineCommand "frac" [] [[MyNum "1"],[MyNum "6"]],Operator "\n",Operator "[",MyNum "0.3",MyStr "em",Operator "]",MyNum "0",Operator "&",InlineCommand "frac" [] [[MyNum "5"],[MyNum "6"]],Operator "&",InlineCommand "frac" [] [[MyNum "1"],[MyNum "6"]]]],"")
    (scan "M = \\begin{matrix} \\frac{5}{6} & \\frac{1}{6} & 0 \\\\ [0.3em] \\frac{5}{6} & 0 & \\frac{1}{6} \\\\[0.3em] 0 & \\frac{5}{6} & \\frac{1}{6} \\end{matrix}"))

test30 :: Test
test30 = TestCase (assertEqual "example: f(n) = \\left\\{ \\begin{array}{l l} n/2 & \\quad \\text{if $n$ is even} \\\\ -(n+1)/2 & \\quad \\text{if $n$ is odd} \\end{array} \\right"
    ([MyStr "f",Operator "(",MyStr "n",Operator ")",Operator "=",CommandBodyless "left",Operator "{",ComplexCommand "array" [MyStr "ll"] [MyStr "n",Operator "/",MyNum "2",Operator "&",CommandBodyless "quad",InlineCommand "text" [] [[MyStr "if",Operator "s",MyStr "niseven"]],Operator "\n",Operator "-",Operator "(",MyStr "n",Operator "+",MyNum "1",Operator ")",Operator "/",MyNum "2",Operator "&",CommandBodyless "quad",InlineCommand "text" [] [[MyStr "if",Operator "s",MyStr "nis",CommandBodyless "o",MyStr "dd"]]],CommandBodyless "right"],"")
    (scan "f(n) = \\left\\{ \\begin{array}[]{l l} n/2 & \\quad \\text{if \\ n is even} \\\\ -(n+1)/2 & \\quad \\text{if \\ n is odd} \\end{array} \\right"))
