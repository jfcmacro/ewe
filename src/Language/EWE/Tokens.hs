module Language.EWE.Tokens where

import Text.Parsec.Pos(SourcePos)

type Tokens = [Token]

type Token = (SourcePos, Tkn)

data Tkn = TknInt Int
         | TknStr String
         | TknOp Op
         | TknAssgn
         | TknCon
         | TknId String
         | TknLPar
         | TknRPar
         | TknLBrk
         | TknRBrk
         | TknOper Char
         | TknCond String
         | TknRerWrd String
             deriving (Show)

