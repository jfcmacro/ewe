module Language.EWE.Parser(pEWE) where

import System.IO
import Control.Monad
import Text.Parsec
import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language
import Language.EWE.AbsSyn


eweDef :: P.LanguageDef st
eweDef = emptyDef
         { P.commentLine     = "#"
         , P.nestedComments  = False
         , P.identStart      = letter
         , P.identLetter     = alphaNum
         , P.reservedOpNames = [">=", ">", "<=", "<", "=", "<>"
                               ,"+", "-", "*", "/", "%", ":="]
         , P.reservedNames   = ["PC"
                               ,"readInt", "writeInt"
                               ,"readStr", "writeStr"
                               ,"goto"
                               ,"if", "then"
                               ,"halt","break"
                               ,"equ", "M"]
         , P.caseSensitive   = True
         }

eweLexer = P.makeTokenParser eweDef

pIdentifier :: Parser String
pIdentifier = P.identifier eweLexer

pReserved :: String -> Parser ()
pReserved = P.reserved eweLexer

pReservedOp :: String -> Parser ()
pReservedOp = P.reservedOp eweLexer

pInteger :: Parser Integer
pInteger = P.integer eweLexer

pBrackets :: Parser a -> Parser a
pBrackets = P.brackets eweLexer

pParens :: Parser a -> Parser a
pParens = P.parens eweLexer

pColon :: Parser String
pColon = P.colon eweLexer

pString :: Parser String
pString = P.stringLiteral eweLexer

pComma :: Parser String
pComma = P.comma eweLexer

pEWE:: String -> String -> IO Prog
pEWE inp name = do
  case (parse pEweProg name inp) of
    Left  err  -> error (show err)
    Right prog -> return prog

pEweProg :: Parser Prog
pEweProg = do stms    <- pStms
              equates <- pEquates
              return $ Prg stms equates

pComment :: Parser ()
pComment = do char '#'
              many (noneOf ['\n','\r'])
              pEOL
              return ()

pStms :: Parser Stmts
pStms = endBy1 pLabelInstr pEOL

pLabel :: Parser String
pLabel = do l  <- letter
            ls <- many letter
            char ':'
	    many space
	    return $ (l:ls)
         <?> "Parsing label"

pLabelInstr :: Parser Stmt
pLabelInstr = do labels <- many pLabel
                 instr <- pInstr
                 return $ Stmt labels instr
              <?> "Parsing label instruction"


pInstr :: Parser Instr
pInstr = do instr <- choice [pPCInstr, pStartWithMRefInstr, pIMMR, pReadInt
                            ,pWriteInt, pReadStr, pWriteStr, pGoto, pIf
                            ,pHalt, pBreak]
            return instr

pEOL :: Parser ()
pEOL = (char '\n' >> return ()) <|> (string "\r\n" >> return ())

pHalt :: Parser Instr
pHalt = do pReserved "halt"
           return (IH)

pBreak :: Parser Instr
pBreak = do pReserved "break"
            return (IB)

pGoto :: Parser Instr
pGoto = do pReserved "goto"
           pEndGoto
           
pEndGoto :: Parser Instr
pEndGoto = (pInteger >>= \i -> return $ IGI (fromInteger i))
         <|> (pIdentifier >>= \s -> return $ IGS s)

pIf :: Parser Instr
pIf = do pReserved "if"
         mra  <- pMRef
         cond <- pCond
         mrb  <- pMRef
         pReserved "then"
         pReserved "goto"
         pIfEnd mra cond mrb

pIfEnd :: MRef -> Cond -> MRef -> Parser Instr
pIfEnd mra cond mbr =
  (pInteger >>= \x -> return $ IFI mra cond mbr (fromInteger x))
  <|>
  (pIdentifier >>= \x -> return $ IFS mra cond mbr x)

pReadInt :: Parser Instr
pReadInt = do pReserved "readInt"
              mr <- pParens pMRef
              return $ IRI mr

pWriteInt :: Parser Instr
pWriteInt = do pReserved "writeInt"
               mr <- pParens pMRef
               return $ IWI mr

pReadStr :: Parser Instr
pReadStr = do pReserved "readStr"
              (mra,mrb) <- pParens pMRefReadStr
              return $ IRS mra mrb

pMRefReadStr :: Parser (MRef,MRef)
pMRefReadStr = do mra <- pMRef
                  pComma
                  mrb <- pMRef
                  return (mra, mrb)

pWriteStr :: Parser Instr
pWriteStr = do pReserved "writeStr"
               mr <- pParens pMRef
               return $ IWS mr

pPCInstr :: Parser Instr
pPCInstr = do pReserved "PC"
              pReservedOp ":="
              mr <- pMRef
              return $ SPC mr

pCond :: Parser Cond
pCond = do pReservedOp "<="
           return $ CLET
        <|>
        do pReservedOp "<"
           return $ CLT
        <|>
        do pReservedOp ">="
           return $ CGET
        <|>
        do pReservedOp ">"
           return $ CGT
        <|>
        do pReservedOp "="
           return $ CE
        <|>
        do pReservedOp "<>"
           return $ CNE

pIMMR :: Parser Instr
pIMMR = do pReserved "M"
           (mr1, i) <- pBrackets pIMMRref
           pReservedOp ":="
           mr2 <- pMRef
           return $ IMMR mr1 mr2 i


pIMMRref :: Parser (MRef, Int)
pIMMRref = do mr <- pMRef
              pReservedOp "+"
              i <- pInteger
              return (mr, (fromInteger i))

pStartWithMRefInstr :: Parser Instr
pStartWithMRefInstr =
  do mr1 <- pMRef
     pReservedOp ":="
     pNextToMRef mr1

pNextToMRef :: MRef -> Parser Instr
pNextToMRef mr1 =
  do i <- pInteger
     return $ IMMI mr1 (fromInteger i)
  <|>
  do s <- pString
     return $ IMMS mr1 s
  <|>
  do pReserved "PC"
     pReservedOp "+"
     i <- pInteger
     return $ IMRPC mr1 (fromInteger i)
  <|>
  do mr2 <- pMRef
     option (IMMM mr1 mr2) (pNextTo2MRef mr1 mr2)


pNextTo2MRef :: MRef -> MRef -> Parser Instr
pNextTo2MRef mr1 mr2 =
  do pReservedOp "+"
     mr3 <- pMRef
     return $ IAdd mr1 mr2 mr3
  <|>
  do pReservedOp "-"
     mr3 <- pMRef
     return $ ISub mr1 mr2 mr3
  <|>
  do pReservedOp "*"
     mr3 <- pMRef
     return $ IMul mr1 mr2 mr3
  <|>
  do pReservedOp "/"
     mr3 <- pMRef
     return $ IDiv mr1 mr2 mr3
  <|>
  do pReservedOp "%"
     mr3 <- pMRef
     return $ IMod mr1 mr2 mr3

pMRef :: Parser MRef
pMRef = do pReserved "M"
           i <- pBrackets pInteger
           return $ MRefI (fromInteger i)
        <|>
        do id <- pIdentifier
           return $ MRefId id

pEquates :: Parser Equates
pEquates = many pEqu >>= \equates -> return equates

pEqu :: Parser Equ
pEqu = do pReserved "equ"
          id <- pIdentifier
          pReserved "M"
          i <- pBrackets pInteger
          return (id, (fromInteger i))
