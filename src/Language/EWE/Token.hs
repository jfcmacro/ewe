module Language.EWE.Token(Tkns,Tkn(..)) where

type Tkns = [Tkn]

data Tkn = TknInt   Int
         | TknStr   String
         | TknLabel String
         | TknId    String
         | TknAssgn
         | TknComma
         | TknLPar
         | TknRPar
         | TknLBrk
         | TknRBrk
         | TknOper   Char
         | TknCond   String
         | TknResWrd String
         | TknEOL
             deriving (Show, Eq)
