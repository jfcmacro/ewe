module Language.EWE.Token(Tkns,Tkn(..),Tkn_(..)) where

type Tkns = [Tkn]

type TknPos = (Int,Int)

data Tkn  = Tkn TknPos Tkn_ deriving (Show, Eq)

data Tkn_ = TknInt   Int
          | TknStr   String
          | TknId    String
          | TknAssgn
          | TknColon
          | TknComma
          | TknLPar
          | TknRPar
          | TknLBrk
          | TknRBrk
          | TknOper   Char
          | TknCond   String
          | TknResWrd String
          | TknEOL
          | TknEOF
             deriving (Show, Eq)
