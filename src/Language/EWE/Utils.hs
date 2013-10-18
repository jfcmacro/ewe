module Language.EWE.Utils(emptyProg,
                          equates,
                          stms,
                          convert) where

import Language.EWE.AbsSyn

emptyProg :: Prog
emptyProg = Prg [] []

equates :: Prog -> Equates
equates (Prg _ e) = e

stms :: Prog -> Stmts
stms (Prg s _) = s

convert :: Either String () -> Either String () -> Either String ()
convert (Left s) (Left s') = Left (s ++ s')
convert (Left s) _         = Left s
convert _        (Left s') = Left s'
convert _        _         = Right ()
