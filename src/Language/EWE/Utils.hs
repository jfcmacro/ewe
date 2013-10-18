module Language.EWE.Utils(emptyProg, equates, stms) where

import Language.EWE.AbsSyn

emptyProg :: Prog
emptyProg = Prg [] []

equates :: Prog -> Equates
equates (Prg _ e) = e

stms :: Prog -> Stmts
stms (Prg s _) = s
