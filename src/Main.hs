module Main where

import System.Exit
import Test.HUnit
import Latex2MathML.Scanner.Main
import Latex2MathML.Scanner.Tests
import Latex2MathML.Parser.Main

main :: IO a
main = do
    _ <- runTestTT tests
    --print $ scan "\\{ \\["
    --print $ scan "\\forall x \\in X, \\quad \\exists y \\leq \\epsilon"
    --print $ scan "\\int\\limits_a^b"
    --putStrLn $ show  $ parse $ fst $ scan "2+    2 \\frac{1}{2} \\begin{matrix} 2 & \\alpha \\\\ 3 & 4 \\end{matrix} + 2"
    --putStrLn (show (scan "2+    2 \\frac{1}{2} \\begin{matrix} 2 & \\alpha \\\\ A & B \\end{matrix} + 2"))

    print $ scan "\\( 12             13 \\)\\"
    -- Wykrywa liczby jako osobne elemnty - w LaTeX'u spacje są w tym przypadku pomijane, powinien wypisać 1213.
    -- trzeba to będzie uwzględnić przy generowaniu, jeśli nie pojawi się wymuszona spacja (\(spacja)) to można wypisać liczby razem

    print $ scan "\\( 12 \\ 13 \\)\\"
    -- Wykrywa CommandBodyless " 13", powinien wykryć spację i numer, BTW. wykrywanie spacji nie jest chyba uwzględnione w gramatyce skanera

    print $ scan "x^\\frac{1}{\\frac{2}{3}}"
    -- ([MyStr "x",Sup [],MyStr "rac"],"{1}{\frac{2}{3}}\n") ? Problem z drugim frac

    print $ scan "\\frac 1 2" -- jest ok
    print $ scan "\\frac12"
    -- ([CommandBodyless "frac12\n"],"") - wyrażenie jest poprawne z punktu widzenia LaTeX'a.
    -- trzeba jakoś wymusić żeby po natrafieniu na frac resztę traktował jako argumenty

    print $ scan "1^2^3"
    print $ scan "1_2_3"
    -- Dwa powyższe skanuje prawidłowo. Oczywiście nie są poprawne, ale tutaj to nie ma znaczenia
    print $ scan "1^2_3" -- jest ok
    exitSuccess

