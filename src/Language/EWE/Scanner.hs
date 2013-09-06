module Language.EWE.Scanner where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Pos(SourcePos)
import Language.Calc.Token(Tokens, Token, Tok(..))
import Control.Monad.State(get)

getPos :: Parser SourcePos
getPost = statePos <$> getParserState

pTknInt :: Parser Token
pTknInt = do ini <- getPos 
             i  <- oneOf "123456789" 
             is <- many digit
             return $ (ini, TknInt (read (i:is)))


pTknChar :: Char -> Tkn -> Parser Token
pTknChar c tkn = do \x _ -> (x,tkn) <$> getPos <*> oneOf [c]

pTknLPar, pTknRPar, pTknLBrk, pTknRBrk :: Parser Token
pTknLPar = pTnkChar '(' TknLPar
pTknRPar = pTknChar ')' TknRPar
pTknLBrk = pTknChar '[' TknLBrk
pTknRBrk = pTknChar ']' TknRBrk

