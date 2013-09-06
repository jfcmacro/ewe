module Language.EWE.Scanner where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Pos(SourcePos)
import Language.Calc.Token(Tokens, Token, Tok(..))
import Control.Monad.State(get)

pTknChar :: Char -> Tkn -> Parser Token
pTknChar c tkn = do (,) <$> getPos <*> oneOf [c]

pTknLPar, pTknRPar, pTknLBrk, pTknRBrk :: Parser Token
pTknLPar = pTnkChar '(' TknLPar
pTknRPar = pTknChar ')' TknRPar
pTknLBrk = pTknChar '[' TknLBrk
pTknRBrk = pTknChar ']' TknRBrk
