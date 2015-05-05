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
                  TestLabel "testAcc" testAcc, TestLabel "test31" test31, TestLabel "test32" test32]

testGr1 :: Test
testGr1 = TestCase (assertEqual "Greek symbols, part one"
    (Right [Command "alpha", Command "beta", Command "Gamma", Command "gamma", Command "Delta",
    Command "delta", Command "epsilon", Command "varepsilon", Command "zeta", Command "eta",
    Command "Theta", Command "theta", Command "vartheta", Command "iota", Command "kappa",
    Command "lambda", Command "Lambda", Command "mu"])
    (scan "\\alpha \\beta \\Gamma \\gamma \\Delta \\delta \\epsilon \\varepsilon \\zeta \\eta \\Theta \\theta \\vartheta \\iota \\kappa \\lambda \\Lambda \\mu"))

testGr2 :: Test
testGr2 = TestCase (assertEqual "Greek symbols, part two"
    (Right [Command "nu", Command "Xi", Command "xi", Command "Pi", Command "pi",
     Command "varpi", Command "rho", Command "varrho", Command "Sigma", Command "sigma",
     Command "varsigma", Command "tau", Command "Upsilon", Command "upsilon", Command "Phi",
     Command "phi", Command "varphi", Command "chi", Command "Psi", Command "psi",
     Command "Omega", Command "omega", Command "omicron"])
    (scan "\\nu \\Xi \\xi \\Pi \\pi \\varpi \\rho \\varrho \\Sigma \\sigma \\varsigma \\tau \\Upsilon \\upsilon \\Phi \\phi \\varphi \\chi \\Psi \\psi \\Omega \\omega \\omicron"))

testOth :: Test
testOth = TestCase (assertEqual "Other symbols class"
    (Right [Command "partial", Command "imath", Command "Re", Command "nabla", Command "aleph",
    Command "eth", Command "jmath", Command "Im", Command "Box", Command "beth",
    Command "hbar", Command "ell", Command "wp", Command "infty", Command "gimel"])
    (scan "\\partial \\imath \\Re \\nabla \\aleph \\eth \\jmath \\Im \\Box \\beth \\hbar \\ell \\wp \\infty \\gimel"))

testTri :: Test
testTri = TestCase (assertEqual "Trigonometric symbols"
    (Right [Command "sin", Command "arcsin", Command "sinh", Command "sec", Command "cos",
    Command "arccos", Command "cosh", Command "csc", Command "tan", Command "arctan",
    Command "tanh", Command "cot", Command "arccot", Command "coth"])
    (scan "\\sin \\arcsin \\sinh \\sec \\cos \\arccos \\cosh \\csc \\tan \\arctan \\tanh \\cot \\arccot \\coth"))

testSet :: Test
testSet = TestCase (assertEqual "Set and logic symbols"
    (Right [Command "exists", Command "nexists", Command "forall", Command "neg", Command "subset",
    Command "supset", Command "in", Command "notin", Command "ni", Command "lor",
    Command "rightarrow", Command "to", Command "leftarrow", Command "gets", Command "mapsto",
    Command "implies", Command "Rightarrow", Command "leftrightarrow", Command "iff", Command "Leftrightarrow",
    Command "top", Command "land", Command "bot", Command "emptyset", Command "varnothing"])
    (scan "\\exists \\nexists \\forall \\neg \\subset \\supset \\in \\notin \\ni \\lor \\rightarrow \\to \\leftarrow \\gets \\mapsto \\implies \\Rightarrow \\leftrightarrow \\iff \\Leftrightarrow \\top \\land \\bot \\emptyset \\varnothing"))

testDel :: Test
testDel = TestCase (assertEqual "Delimiter symbols"
    (Right [Command "uparrow", Command "downarrow", Operator "}", Command "doubleOr", Operator "|", Command "Uparrow",
    Command "Downarrow", Operator "/", Command "langle", Command "lceil", Command "lfloor",
    Command "backslash", Command "rangle", Command "rceil", Command "rfloor"])
    (scan "\\uparrow \\downarrow \\} \\| | \\Uparrow \\Downarrow / \\langle \\lceil \\lfloor \\backslash \\rangle \\rceil \\rfloor"))
-- \\| Command "doubleOr",
-- Problems with recognizing command \| (rendered as ||)
-- \\} - in this context it shouldn't it be a command?

testBin :: Test
testBin = TestCase (assertEqual "Binary symbols"
    (Right [Command "pm", Command "cap", Command "diamond", Command "oplus", Command "mp",
    Command "cup", Command "ominus", Command "times", Command "uplus", Command "bigtriangledown",
    Command "bigtriangleup", Command "otimes", Command "div", Command "sqcap", Command "triangleleft",
    Command "oslash", Command "ast", Command "sqcup", Command "triangleright", Command "odot",
    Command "star", Command "vee", Command "bigcirc", Command "circ", Command "dagger",
    Command "wedge", Command "bullet", Command "setminus", Command "ddagger", Command "cdot",
    Command "wr", Command "amalg"])
    (scan "\\pm \\cap \\diamond \\oplus \\mp \\cup \\ominus \\times \\uplus \\bigtriangledown \\bigtriangleup \\otimes \\div \\sqcap \\triangleleft \\oslash \\ast \\sqcup \\triangleright \\odot \\star \\vee \\bigcirc \\circ \\dagger \\wedge \\bullet \\setminus \\ddagger \\cdot \\wr \\amalg"))

testRel :: Test
testRel = TestCase (assertEqual "Relation symbols"
    (Right [Command "parallel", Command "nparallel", Command "leq", Command "geq", Command "doteq",
    Command "asymp", Command "bowtie", Command "ll", Command "gg", Command "equiv",
    Command "vdash", Command "dashv", Command "subset", Command "supset", Command "approx",
    Command "in", Command "ni", Command "subseteq", Command "supseteq", Command "cong",
    Command "smile", Command "frown", Command "nsubseteq", Command "nsupseteq", Command "sqsubset",
    Command "sqsupset", Command "simeq", Command "models", Command "notin", Command "sim",
    Command "perp", Command "mid", Command "sqsubseteq", Command "sqsupseteq", Command "propto",
    Command "prec", Command "succ", Command "preceq", Command "succeq", Command "neq"])
    (scan "\\parallel \\nparallel \\leq \\geq \\doteq \\asymp \\bowtie \\ll \\gg \\equiv \\vdash \\dashv \\subset \\supset \\approx \\in \\ni \\subseteq \\supseteq \\cong \\smile \\frown \\nsubseteq \\nsupseteq \\sqsubset \\sqsupset \\simeq \\models \\notin \\sim \\perp \\mid \\sqsubseteq \\sqsupseteq \\propto \\prec \\succ \\preceq \\succeq \\neq"))


testMath :: Test
testMath = TestCase (assertEqual "Test for Math symbols"
    (Right [Command "prod",Command "sum",Command "lim",Command "int"])
    (scan "\\prod \\sum \\lim \\int "))

testAcc :: Test
testAcc = TestCase (assertEqual "Test for Accent symbols"
    (Right [Command "hat",Command "grave",Command "bar",Command "acute",Command "mathring",Command "check",Command "dot",Command "vec",Command "breve",Command "tilde",Command "ddot",Command "widehat",Command "widetilde"])
    (scan "\\hat \\grave \\bar \\acute \\mathring \\check \\dot \\vec \\breve \\tilde \\ddot \\widehat \\widetilde"))

test1 :: Test
test1 = TestCase (assertEqual "Testing simple Sup; expression: k^2"
    (Right [MyStr "k",Sup, MyNum "2"])
    (scan "k^2"))

test2 :: Test
test2 = TestCase (assertEqual "Nested commands; expression: \\frac{1 \\frac{ \\frac{2 + 3 - 4}{3}}{4}}{5}"
    (Right [MyStr "aaa", BodyBegin, MyNum "1",Command "frac", BodyBegin, Command "frac", BodyBegin, MyNum "2",Operator "+",MyNum "3",Operator "-",MyNum "4", BodyEnd,BodyBegin, MyNum "3",BodyEnd,BodyEnd, BodyBegin,MyNum "4",BodyEnd,BodyEnd,BodyBegin,MyNum "5",BodyEnd])
    (scan "aaa{1 \\frac{ \\frac{2 + 3 - 4}{3}}{4}}{5}"))

test3 :: Test
test3 = TestCase (assertEqual "Operators and Brackets; expression: =+*/!<>|:() ' '' ''' ''''"
    (Right [Operator "=",Operator "+",Operator "*",Operator "/",Operator "!",Operator "<",Operator ">",Operator "|",Operator ":",Operator "(",Operator ")",Operator "'",Operator "''",Operator "'''",Operator"''''"])
    (scan "=+*/!<>|:() ' '' ''' ''''"))

test4 :: Test
test4 = TestCase (assertEqual "Sup with command; expression: x^\\frac{1}{2}"
    (Right [MyStr "x",Sup,Command "frac", BodyBegin, MyNum "1",BodyEnd,BodyBegin,MyNum "2",BodyEnd])
    (scan "x^\\frac{1}{2}"))

test5 :: Test
test5 = TestCase (assertEqual "Sup and Sub test; expression: k_{n+1} = n^2 + k_n^2 - k_{n-1}"
    (Right [MyStr "k",Sub, BodyBegin, MyStr "n",Operator "+",MyNum "1",BodyEnd, Operator "=",MyStr "n",Sup,MyNum "2",Operator "+",MyStr "k",Sub,MyStr "n",Sup,MyNum "2",Operator "-",MyStr "k",Sub,BodyBegin,MyStr "n",Operator "-",MyNum "1",BodyEnd])
    (scan "k_{n+1} = n^2 + k_n^2 - k_{n-1}"))

test6 :: Test
test6 = TestCase (assertEqual "Inconvinient comment; expression: \\frac{2}%stupid place for comment \n{3}"
    (Right [Command "frac", BodyBegin, MyNum "2",BodyEnd, BodyBegin, MyNum "3",BodyEnd])
    (scan "\\frac{2}%stupid place for comment \n{3}"))

test7 :: Test
test7 = TestCase (assertEqual "Square bracket testing; expression: [] \\sqrt[n]{1}{2}"
    (Right [Operator "[",Operator "]", Command "sqrt", Operator "[",MyStr "n",Operator "]",BodyBegin,MyNum "1",BodyEnd,BodyBegin,MyNum "2",BodyEnd])
    (scan "[] \\sqrt[n]{1}{2}"))

test8 :: Test
test8 = TestCase (assertEqual "Complex Command testing; expression: \\begin{matrix} \\alpha & 2 \\\\ 3 & 4 \\end{matrix}"
    (Right [Command "begin",BodyBegin,MyStr "matrix",BodyEnd, Command "alpha",Operator "&",MyNum "2",Operator "\n",MyNum "3",Operator "&",MyNum "4",Command "end",BodyBegin,MyStr "matrix",BodyEnd])
    (scan "\\begin{matrix} \\alpha & 2 \\\\ 3 & 4 \\end{matrix}"))

test9 :: Test
test9 = TestCase (assertEqual "Number string with unncessary spaces: 12             13 "
    (Right [MyNum "1213"])
    (scan "12             13"))

test10 :: Test
test10 = TestCase (assertEqual "Number string with single space: 12 \\ 13"
        (Right [MyNum "12",Operator "s",MyNum "13"])
        (scan "12 \\ 13"))

test11 :: Test
test11 = TestCase (assertEqual "Fraction without brackets 1: \\frac 1 2"
        (Right [Command "frac",MyNum "1",MyNum "2"])
        (scan "\\frac 1 2"))

test12 :: Test
test12 = TestCase (assertEqual "Fraction without brackets 2: \\frac12"
        (Right [Command "frac",MyNum "1",MyNum "2"])
        (scan "\\frac12"))

test13 :: Test
test13 = TestCase (assertEqual "Sum symbol, without arguments: \\sum{12}{34}"
        (Right [Command "sum", BodyBegin, MyNum "12",BodyEnd,BodyBegin,MyNum "34",BodyEnd])
        (scan "\\sum{12}{34}"))

test14 :: Test
test14 = TestCase (assertEqual "Sum symbol, with arguments: \\sum_{12}^{34}"
        (Right [Command "sum",Sub,BodyBegin,MyNum "12",BodyEnd,Sup,BodyBegin,MyNum "34",BodyEnd])
        (scan "\\sum_{12}^{34}"))

test15 :: Test
test15 = TestCase (assertEqual "wikipedia example: \\forall x \\in X, \\quad \n \\exists y \\leq \\epsilon"
    (Right [Command "forall",MyStr "x",Command "in",Command "X",Operator ",",Command "quad",Command "exists",MyStr "y",Command "leq",Command "epsilon"])
    (scan "\\forall x \\in X, \\quad \n \\exists y \\leq \\epsilon"))

test16 :: Test
test16 = TestCase (assertEqual "wikipedia example (greek letters): \\alpha, \\Alpha, \\beta, \\Beta, \\gamma, \\Gamma, \\pi, \\Pi, \\phi, \\varphi, \\Phi"
    (Right [Command "alpha",Operator ",",Command "Alpha",Operator ",",Command "beta",Operator ",",Command "Beta",Operator ",",Command "gamma",Operator ",",Command "Gamma",Operator ",",Command "pi",Operator ",",Command "Pi",Operator ",",Command "phi",Operator ",",Command "varphi",Operator ",",Command "Phi"])
    (scan "\\alpha, \\Alpha, \\beta, \\Beta, \\gamma, \\Gamma, \\pi, \\Pi, \\phi, \\varphi, \\Phi"))

test17 :: Test
test17 = TestCase (assertEqual "wikipedia example (operators 1): \\cos (2\\theta) = \\cos^2 \\theta - \\sin^2 \\theta"
    (Right [Command "cos",Operator "(",MyNum "2",Command "theta",Operator ")",Operator "=",Command "cos",Sup, MyNum "2",Command "theta",Operator "-",Command "sin",Sup, MyNum "2",Command "theta"])
    (scan "\\cos (2\\theta) = \\cos^2 \\theta - \\sin^2 \\theta"))

test18 :: Test
test18 = TestCase (assertEqual "wikipedia example (operators 2): \\lim_{x \\to \\infty} \\exp(-x) = 0"
    (Right [Command "lim",Sub,BodyBegin, MyStr "x", Command "to", Command "infty",BodyEnd,Command "exp", Operator "(",Operator "-",MyStr "x",Operator ")",Operator "=",MyNum "0"])
    (scan "\\lim_{x \\to \\infty} \\exp(-x) = 0"))

test19 :: Test
test19 = TestCase (assertEqual "wikipedia example: f(n) = n^5 + 4n^2 + 2 |_{n=17}"
    (Right [MyStr "f",Operator "(",MyStr "n",Operator ")",Operator "=",MyStr "n",Sup,MyNum "5",Operator "+",MyNum "4",MyStr "n",Sup,MyNum "2",Operator "+",MyNum "2",Operator "|",Sub,BodyBegin,MyStr "n",Operator "=",MyNum "17",BodyEnd])
    (scan "f(n) = n^5 + 4n^2 + 2 |_{n=17}"))

test20 :: Test
test20 = TestCase (assertEqual "example: \frac{n!}{k!(n-k)!} = \binom{n}{k}"
    (Right [Command "frac",BodyBegin,MyStr "n",Operator "!",BodyEnd,BodyBegin,MyStr "k",Operator "!",Operator "(",MyStr "n",Operator "-",MyStr "k",Operator ")",Operator "!",BodyEnd,Operator "=",Command "binom",BodyBegin,MyStr "n",BodyEnd,BodyBegin,MyStr "k",BodyEnd])
    (scan "\\frac{n!}{k!(n-k)!} = \\binom{n}{k}"))

test21 :: Test
test21 = TestCase (assertEqual "example: \\frac{\\frac{1}{x}+\\frac{1}{y}}{y-z}"
    (Right [Command "frac",BodyBegin,Command "frac",BodyBegin,MyNum "1",BodyEnd,BodyBegin,MyStr "x",BodyEnd,Operator "+",Command "frac",BodyBegin,MyNum "1",BodyEnd,BodyBegin,MyStr "y",BodyEnd,BodyEnd,BodyBegin,MyStr "y",Operator "-",MyStr "z",BodyEnd])
    (scan "\\frac{\\frac{1}{x}+\\frac{1}{y}}{y-z}"))

test22 :: Test
test22 = TestCase (assertEqual "example: x = a_0 + \\cfrac{1}{a_1 + \\cfrac{1}{a_2 + \\cfrac{1}{a_3 + \\cfrac{1}{a_4}}}}"
    (Right [MyStr "x",Operator "=",MyStr "a",Sub,MyNum "0",Operator "+",Command "cfrac",BodyBegin,MyNum "1",BodyEnd,BodyBegin,MyStr "a",Sub,MyNum "1",Operator "+",Command "cfrac",BodyBegin,MyNum "1",BodyEnd,BodyBegin,MyStr "a",Sub,MyNum "2",Operator "+",Command "cfrac",BodyBegin,MyNum "1",BodyEnd,BodyBegin,MyStr "a",Sub,MyNum "3",Operator "+",Command "cfrac",BodyBegin,MyNum "1",BodyEnd,BodyBegin,MyStr "a",Sub,MyNum "4",BodyEnd,BodyEnd,BodyEnd,BodyEnd])
    (scan "x = a_0 + \\cfrac{1}{a_1 + \\cfrac{1}{a_2 + \\cfrac{1}{a_3 + \\cfrac{1}{a_4}}}}"))

test23 :: Test
test23 = TestCase (assertEqual "example: \\frac{\\begin{array}[b]{r}\\left(x_1 x_2 \\right) \\\\ \\times \\left( x'_1 x'_2 \\right) \\end{array}}{\\left( y_1y_2y_3y_4 \\right)}"
    (Right [Command "frac",BodyBegin,Command "begin",BodyBegin,MyStr "array",BodyEnd,Operator "[",MyStr "b",Operator "]",BodyBegin,MyStr "r",BodyEnd,Command "left(",MyStr "x",Sub,MyNum "1",MyStr "x",Sub,MyNum "2",Command "right)",Operator "\n",Command "times",Command "left(",MyStr "x",Operator "'",Sub,MyNum "1",MyStr "x",Operator "'",Sub,MyNum "2",Command "right)",Command "end",BodyBegin,MyStr "array",BodyEnd,BodyEnd,BodyBegin,Command "left(",MyStr "y",Sub,MyNum "1",MyStr "y",Sub,MyNum "2",MyStr "y",Sub,MyNum "3",MyStr "y",Sub,MyNum "4",Command "right)",BodyEnd])
    (scan "\\frac{\\begin{array}[b]{r}\\left(x_1 x_2 \\right) \\\\ \\times \\left( x'_1 x'_2 \\right) \\end{array}}{\\left( y_1y_2y_3y_4 \\right)}"))

test24 :: Test
test24 = TestCase (assertEqual "example: \\sqrt[n]{1+x+x^2+x^3+\\ldots}"
    (Right [Command "sqrt",Operator "[",MyStr "n",Operator "]",BodyBegin,MyNum "1",Operator "+",MyStr "x",Operator "+",MyStr "x",Sup,MyNum "2",Operator "+",MyStr "x",Sup,MyNum "3",Operator "+",Command "ldots",BodyEnd])
    (scan "\\sqrt[n]{1+x+x^2+x^3+\\ldots}"))

test25 :: Test
test25 = TestCase (assertEqual "example: \\int_0^\\infty \\mathrm{e}^{-x} \\mathrm{d}x"
    (Right [Command "int",Sub,MyNum "0",Sup,Command "infty",Command "mathrm",BodyBegin,MyStr "e",BodyEnd,Sup,BodyBegin,Operator "-",MyStr "x",BodyEnd,Command "mathrm",BodyBegin,MyStr "d",BodyEnd,MyStr "x"])
    (scan "\\int_0^\\infty \\mathrm{e}^{-x} \\mathrm{d}x"))

test26 :: Test
test26 = TestCase (assertEqual "example: \\begin{matrix} a & b & c \\\\ d & e & f \\\\ g & h & i \\end{matrix}"
    (Right [Command "begin",BodyBegin,MyStr "matrix",BodyEnd,MyStr "a",Operator "&",MyStr "b",Operator "&",MyStr "c",Operator "\n",MyStr "d",Operator "&",MyStr "e",Operator "&",MyStr "f",Operator "\n",MyStr "g",Operator "&",MyStr "h",Operator "&",MyStr "i",Command "end",BodyBegin,MyStr "matrix",BodyEnd])
    (scan "\\begin{matrix} a & b & c \\\\ d & e & f \\\\ g & h & i \\end{matrix}"))

test27 :: Test
test27 = TestCase (assertEqual "example: \\begin{matrix} -1 & 3 \\\\ 2 & -4 \\end{matrix} = \\begin{matrix}[r] -1 & 3 \\\\ 2 & -4 \\end{matrix}"
    (Right [Command "begin",BodyBegin,MyStr "matrix",BodyEnd,Operator "-",MyNum "1",Operator "&",MyNum "3",Operator "\n",MyNum "2",Operator "&",Operator "-",MyNum "4",Command "end",BodyBegin,MyStr "matrix",BodyEnd,Operator "=",Command "begin",BodyBegin,MyStr "matrix",BodyEnd,Operator "[",MyStr "r",Operator "]",Operator "-",MyNum "1",Operator "&",MyNum "3",Operator "\n",MyNum "2",Operator "&",Operator "-",MyNum "4",Command "end",BodyBegin,MyStr "matrix",BodyEnd])
    (scan "\\begin{matrix} -1 & 3 \\\\ 2 & -4 \\end{matrix} = \\begin{matrix}[r] -1 & 3 \\\\ 2 & -4 \\end{matrix}"))

test28 :: Test
test28 = TestCase (assertEqual "example: A_{m,n} = \\begin{matrix} a_{1,1} & a_{1,2} & \\cdots & a_{1,n} \\\\ a_{2,1} & a_{2,2} & \\cdots & a_{2,n} \\\\ \\vdots  & \\vdots  & \\ddots & \\vdots  \\\\ a_{m,1} & a_{m,2} \\cdots & a_{m,n} \\end{matrix}"
    (Right [Command "A",Sub,BodyBegin,MyStr "m",Operator ",",MyStr "n",BodyEnd,Operator "=",Command "begin",BodyBegin,MyStr "matrix",BodyEnd,MyStr "a",Sub,BodyBegin,MyNum "1",Operator ",",MyNum "1",BodyEnd,Operator "&",MyStr "a",Sub,BodyBegin,MyNum "1",Operator ",",MyNum "2",BodyEnd,Operator "&",Command "cdots",Operator "&",MyStr "a",Sub,BodyBegin,MyNum "1",Operator ",",MyStr "n",BodyEnd,Operator "\n",MyStr "a",Sub,BodyBegin,MyNum "2",Operator ",",MyNum "1",BodyEnd,Operator "&",MyStr "a",Sub,BodyBegin,MyNum "2",Operator ",",MyNum "2",BodyEnd,Operator "&",Command "cdots",Operator "&",MyStr "a",Sub,BodyBegin,MyNum "2",Operator ",",MyStr "n",BodyEnd,Operator "\n",Command "vdots",Operator "&",Command "vdots",Operator "&",Command "ddots",Operator "&",Command "vdots",Operator "\n",MyStr "a",Sub,BodyBegin,MyStr "m",Operator ",",MyNum "1",BodyEnd,Operator "&",MyStr "a",Sub,BodyBegin,MyStr "m",Operator ",",MyNum "2",BodyEnd,Command "cdots",Operator "&",MyStr "a",Sub,BodyBegin,MyStr "m",Operator ",",MyStr "n",BodyEnd,Command "end",BodyBegin,MyStr "matrix",BodyEnd])
    (scan "A_{m,n} = \\begin{matrix} a_{1,1} & a_{1,2} & \\cdots & a_{1,n} \\\\ a_{2,1} & a_{2,2} & \\cdots & a_{2,n} \\\\ \\vdots  & \\vdots  & \\ddots & \\vdots  \\\\ a_{m,1} & a_{m,2} \\cdots & a_{m,n} \\end{matrix}"))

test29 :: Test
test29 = TestCase (assertEqual "M = \\begin{matrix} \\frac{5}{6} & \\frac{1}{6} & 0 \\\\ [0.3em] \\frac{5}{6} & 0 & \\frac{1}{6} \\\\[0.3em] 0 & \\frac{5}{6} & \\frac{1}{6} \\end{matrix}"
    (Right [Command "M",Operator "=",Command "begin",BodyBegin,MyStr "matrix",BodyEnd,Command "frac",BodyBegin,MyNum "5",BodyEnd,BodyBegin,MyNum "6",BodyEnd,Operator "&",Command "frac",BodyBegin,MyNum "1",BodyEnd,BodyBegin,MyNum "6",BodyEnd,Operator "&",MyNum "0",Operator "\n",Operator "[",MyNum "0.3",MyStr "em",Operator "]",Command "frac",BodyBegin,MyNum "5",BodyEnd,BodyBegin,MyNum "6",BodyEnd,Operator "&",MyNum "0",Operator "&",Command "frac",BodyBegin,MyNum "1",BodyEnd,BodyBegin,MyNum "6",BodyEnd,Operator "\n",Operator "[",MyNum "0.3",MyStr "em",Operator "]",MyNum "0",Operator "&",Command "frac",BodyBegin,MyNum "5",BodyEnd,BodyBegin,MyNum "6",BodyEnd,Operator "&",Command "frac",BodyBegin,MyNum "1",BodyEnd,BodyBegin,MyNum "6",BodyEnd,Command "end",BodyBegin,MyStr "matrix",BodyEnd])
    (scan "M = \\begin{matrix} \\frac{5}{6} & \\frac{1}{6} & 0 \\\\ [0.3em] \\frac{5}{6} & 0 & \\frac{1}{6} \\\\[0.3em] 0 & \\frac{5}{6} & \\frac{1}{6} \\end{matrix}"))

test30 :: Test
test30 = TestCase (assertEqual "example: f(n) = \\left\\{ \\begin{array}{l l} n/2 & \\quad \\text{if $n$ is even} \\\\ -(n+1)/2 & \\quad \\text{if $n$ is odd} \\end{array} \\right\\}"
    (Right [MyStr "f",Operator "(",MyStr "n",Operator ")",Operator "=",Command "left{",Command "begin",BodyBegin,MyStr "array",BodyEnd,Operator "[",Operator "]",BodyBegin,MyStr "ll",BodyEnd,MyStr "n",Operator "/",MyNum "2",Operator "&",Command "quad",Command "text",BodyBegin,MyStr "if",Operator "s",MyStr "niseven",BodyEnd,Operator "\n",Operator "-",Operator "(",MyStr "n",Operator "+",MyNum "1",Operator ")",Operator "/",MyNum "2",Operator "&",Command "quad",Command "text",BodyBegin,MyStr "if",Operator "s",MyStr "nis",Command "o",MyStr "dd",BodyEnd,Command "end",BodyBegin,MyStr "array",BodyEnd,Command "right}"])
    (scan "f(n) = \\left\\{ \\begin{array}[]{l l} n/2 & \\quad \\text{if \\ n is even} \\\\ -(n+1)/2 & \\quad \\text{if \\ n is odd} \\end{array} \\right\\}"))

test31 :: Test
test31 = TestCase (assertEqual "test using newline inside command: \\frac{1}%fdsfdsa \n{2}"
    (Right [Command "frac", BodyBegin, MyNum "1",BodyEnd,BodyBegin,MyNum "2",BodyEnd])
    (scan "\\frac\n{1}%fdsafdsa \n{2} \n"))

test32 :: Test
test32 = TestCase (assertEqual "array with lines example "
    (Right [Command "begin",BodyBegin,MyStr "array",BodyEnd,BodyBegin,MyStr "c",Operator "|",MyStr "c",BodyEnd,MyNum "1",Operator "&",
    MyNum "2",Operator "\n",Command "hline",MyNum "3",Operator "&",MyNum "4",Command "end",BodyBegin,MyStr "array",BodyEnd])
    (scan "\\begin{array}{c | c} 1 & 2 \\\\ \\hline 3 & 4 \\end{array}"))