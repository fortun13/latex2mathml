module Latex2MathML.Generator.Main where

import Latex2MathML.Utils.Definitions
import Data.Map

generate :: [Token] -> String
generate [] = ""
generate lst@(h:t) = getMathml h ++ generate t

getMathml :: Token -> String
getMathml (Operator o) = genmap ! o
getMathml (MyNum num) = "<mn>" ++ num ++ "</mn>"
getMathml (MyStr "") = ""
getMathml (MyStr (h:t)) = "<mi>" ++ [h] ++ "</mi>" ++ getMathml (MyStr t)
getMathml (CommandBodyless com) = genmap ! com
getMathml (InlineCommand com par [bodies]) = ""

genmap :: Map String String
genmap = fromList [("A","<mi>&Alpha;</mi>"), ("B","<mi>&Beta;</mi>"), ("Gamma","<mi>&Gamma;</mi>"), ("Delta","<mi>&Delta;</mi>"), ("E","<mi>&Epsilon;</mi>"), ("Z","<mi>&Zeta;</mi>"), ("H","<mi>&Eta;</mi>"), ("Theta","<mi>&Theta;</mi>"), ("I","<mi>&Iota;</mi>"), ("K","<mi>&Kappa;</mi>"), ("Lambda","<mi>&Lambda;</mi>"), ("M","<mi>&Mu;</mi>"), ("N","<mi>&Nu;</mi>"), ("Xi","<mi>&Xi;</mi>"), ("O","<mi>&Omicron;</mi>"), ("Pi","<mi>&Pi;</mi>"), ("P","<mi>&Rho;</mi>"), ("Sigma","<mi>&Sigma;</mi>"), ("T","<mi>&Tau;</mi>"), ("Upsilon","<mi>&Upsilon;</mi>"), ("Phi","<mi>&Phi;</mi>"), ("X","<mi>&Chi;</mi>"), ("Psi","<mi>&Psi;</mi>"), ("Omega","<mi>&Omega;</mi>"),
    ("alpha","<mi>&alpha;</mi>"), ("beta","<mi>&beta;</mi>"), ("gamma","<mi>&gamma;</mi>"), ("delta","<mi>&delta;</mi>"), ("epsilon","<mi>&epsilon;</mi>"), ("zeta","<mi>&zeta;</mi>"), ("eta","<mi>&eta;</mi>"), ("theta","<mi>&theta;</mi>"), ("iota","<mi>&iota;</mi>"), ("kappa","<mi>&kappa;</mi>"), ("lambda","<mi>&lambda;</mi>"), ("mu","<mi>&mu;</mi>"), ("nu","<mi>&nu;</mi>"), ("xi","<mi>&xi;</mi>"), ("o","<mi>&omicron;</mi>"), ("pi","<mi>&pi;</mi>"), ("rho","<mi>&rho;</mi>"), ("sigma","<mi>&sigma;</mi>"), ("tau","<mi>&tau;</mi>"), ("upsilon","<mi>&upsilon;</mi>"), ("phi","<mi>&phi;</mi>"), ("chi","<mi>&chi;</mi>"), ("psi","<mi>&psi;</mi>"), ("omega","<mi>&omega;</mi>"),
    ("=","<mo>=</mo>"),("-","<mo>&minus;</mo>"), ("*","<mo>&times;</mo>"), ("div","<mo>&divide;</mo>"), ("neq","<mo>&ne;</mo>"), ("approx","<mo>&asymp;</mo>"), ("<","<mo>&lt;</mo>"), ("leq","<mo>&le;</mo>"), (">","<mo>&gt;</mo>"), ("geq","<mo>&ge;</mo>"), ("pm","<mo>&plusmn;</mo>"), ("propto","<mo>&prop;</mo>"), ("sum","<mo>&sum;</mo>"), ("prod","<mo>&prod;</mo>"), ("lfloor","<mo>&lfloor;</mo>"), ("rfloor","<mo>&rfloor;</mo>"), ("lceil","<mo>&lceil;</mo>"), ("rceil","<mo>&rceil;</mo>"),
    ("'","<mo>&prime;</mo>"), ("''","<mo>&Prime;</mo>"), ("'''","<mo>&tprime;</mo>"), ("''''","<mo>&qprime;</mo>"), ("partial","<mo>&part;</mo>"), ("Delta","<mo>&Delta;</mo>"), ("nabla","<mo>&Del;</mo>"), ("int","<mo>&int;</mo>"), ("iint","<mo>&Int;</mo>"), ("iiint","<mo>&tint;</mo>"), ("iiiint","<mo>&qint;</mo>"), ("oint","<mo>&conint;</mo>"), ("varointclockwise","<mo>&cwconint;</mo>"), ("ointctrclockwise","<mo>&awconint;</mo>"), ("oiint","<mo>&Conint;</mo>"), ("oiiint","<mo>&Cconint;</mo>"), ("infty","<mo>&infin;</mo>"),
    ("dots","<mo>&hellip;</mo>"), ("vdots","<mo>&vellip;</mo>"), ("cdots","<mo>&ctdot;</mo>"), ("udots","<mo>&utdot;</mo>"), ("ddots","<mo>&dtdot;</mo>")]
