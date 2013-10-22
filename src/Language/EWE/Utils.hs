module Language.EWE.Utils(emptyProg,
                          equates,
                          stms,
                          (<-->),
                          isMemRefId) where

import Language.EWE.AbsSyn

emptyProg :: Prog
emptyProg = Prg [] []

equates :: Prog -> Equates
equates (Prg _ e) = e

stms :: Prog -> Stmts
stms (Prg s _) = s

infixl 5 <-->

(<-->) :: Either String () -> Either String () -> Either String ()
(Left s) <--> (Left s') = Left (s ++ "\n" ++ s')
(Left s) <--> _         = Left s
_       <--> (Left s')  = Left s'
_       <--> a          = a

isMemRefId :: MRef -> Bool
isMemRefId (MRefId _) = True
