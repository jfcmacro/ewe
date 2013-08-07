module Language.EWE.Parser where

import System.IO
import Control.Monad
import Text.Parsec
import Text.Parsec.Prim
import Text.ParserCombinators.Parsec hiding(try)
import Language.EWE.AbsSyn
import Language.EWE.EweLang(eweLangDef)
import qualified Language.EWE.Token as Tkn
                    
-- Parser generates from the language
pEweLexer = Tkn.makeTokenParser eweLangDef

pIdentifier = Tkn.identifier pEweLexer
pReserved   = Tkn.reserved   pEweLexer
pReservedOp = Tkn.reservedOp pEweLexer
pParens     = Tkn.parens     pEweLexer
pBrackets   = Tkn.brackets   pEweLexer
pInteger    = Tkn.integer    pEweLexer
pComma      = Tkn.comma      pEweLexer
pWhiteSpace = Tkn.whiteSpace pEweLexer
pSymbol     = Tkn.symbol     pEweLexer
pLexeme     = Tkn.lexeme     pEweLexer

-- Parser of EWE Language
pEWE:: String -> String -> IO Prog
pEWE inp name = do
  case (parse pEweProg name inp) of
    Left  err  -> error (show err)
    Right prog -> return prog

pEweProg :: Parser Prog
pEweProg = do stms    <- pStmts
              equates <- pEquates
              return $ Prg stms equates

pStmts :: Parser Stmts
pStmts = many1 pStmtLine

pLabel :: Parser String
pLabel = do id <- pId
            char ':'
	    return $ id
         <?> "Parsing label"

pStmtLine :: Parser Stmt
pStmtLine = do l <- pLabels
               char '\t'
               ins <- pInstr
               pEOL
               return $ Stmt l ins

pLabels :: Parser [String]
pLabels = pLabel `sepBy` pWhiteSpace


-- Instructions parsers
-- <instr> ::= ...
pInstr :: Parser Instr
pInstr = do instr <- choice [pPCInstr
                            ,pReadInt
                            ,pReadStr
                            ,pWriteInt
                            ,pWriteStr
                            ,pGoto
                            ,pIf
                            ,pOneKw
                            ,pPrefixMRef
                            ]
            return instr

-- "PC" ":=" <memref>
-- AbsSyn SPC (Set PC instruction)
pPCInstr :: Parser Instr
pPCInstr = do pReserved"PC"
              pReservedOp ":="
              mr <- pMRef2
              return $ SPC mr

-- "readInt" "(" <memref> ")"
-- AbsSyn IRI Instruction Read Int
-- "readStr" "(" <memref> ")"
-- AbsSyn IRS Instruction Read String

-- pRead :: Parser Instr
-- pRead = do string "read"
--            (string "Int" >> pReadInt) <|> (string "Str" >> pReadStr)

-- pReadInt :: Parser Instr
-- pReadInt = pInstr1MRef IRI

-- pReadStr :: Parser Instr
-- pReadStr = do pSpaces
--               (mr1, mr2) <- pTokenB pReadMRefs
--               pSpaces
--               return $ IRS mr1 mr2
pReadInt :: Parser Instr
pReadInt = do pReserved "readInt"
              mr <- pParens pMRef2
              return $ IRI mr

pReadStr :: Parser Instr
pReadStr = do pReserved "readStr"
              (mr1,mr2) <- pParens p
              return $ IRS mr1 mr2
  where p = do { mr1 <- pMRef2 ; pComma; mr2 <- pMRef2 ; return $ (mr1,mr2) }

-- "writeStr" "(" <memref> ")"
-- AbsSyn IWS Instruction Write String
-- "writeInt" "(" <memref> ")"
-- AbsSyn IWI Instruction Write Int
-- pWrite :: Parser Instr
-- pWrite = do string "write"
--             (string "Int" >> pWriteInt) <|> (string "Str" >> pWriteStr)

-- pWriteInt :: Parser Instr
-- pWriteInt = pInstr1MRef IWI

-- pWriteStr :: Parser Instr
-- pWriteStr = pInstr1MRef IWS
pWriteInt :: Parser Instr
pWriteInt = do pReserved "writeInt"
               mr <- pParens pMRef2
               return $ IWI mr

pWriteStr :: Parser Instr
pWriteStr = do pReserved "writeStr"
               mr <- pParens pMRef2
               return $ IWS mr

-- "goto" Integer
-- "goto" Identifier
-- AbsSyn IGI Goto to line
-- AbsSyn IGS Goto to Symbol
pGoto:: Parser Instr
pGoto = do pReserved "goto"
           iOrId <- pTknIntOrId
           let instr = either IGI IGS iOrId
           return instr

-- "if" <mref> <condition> <memref> "then" "goto" Integer
-- "if" <mref> <condition> <memref> "then" "goto" Identifier
-- AbsSyn IFI If Cond then Int
-- AbsSyn IFS If Cont then Id
pIf :: Parser Instr
pIf = do pReserved "if"
         mr1 <- pMRef2
         cnd <- pCond
         mr2 <- pMRef2
         pReserved "then"
         pReserved "goto"
         iOrId <- pTknIntOrId
         let instr = either (IFI mr1 cnd mr2) (IFS mr1 cnd mr2) iOrId
         return instr

-- "halt"
-- "break"
-- IH Instruction Halt
-- IB Instruciton Break
listOneKw :: [(String,Instr)]
listOneKw = [("halt", IH), ("break", IB)]

pOneKw :: Parser Instr
pOneKw = choice
         $ map (\(s,r) -> (pReserved s >> return r)) listOneKw

pReadMRefs :: Parser (MRef,MRef)
pReadMRefs = do pParens (do mr1 <- pMRef2
                            pComma
                            mr2 <- pMRef2
                            return $ (mr1, mr2))

pInstr1MRef :: (MRef -> Instr) -> Parser Instr
pInstr1MRef f =
  do mr <- pParens pMRef2
     return $ f mr

pPrefixMRef :: Parser Instr
pPrefixMRef =
  do pReserved "M"
     op <- pBrackets pMRefOrIdx'
     pReservedOp ":="
     either pIMMR pNextRHS op
  <|>
  (pLexeme pId >>= \id ->  pReservedOp ":=" >> (pNextRHS (MRefId id)))

pIMMR :: (MRef,Int) -> Parser Instr
pIMMR (mr1,i) =
  do pReservedOp ":="
     mr2 <- pMRef2
     return $ IMMR mr1 i mr2

pNextRHS :: MRef -> Parser Instr
pNextRHS mr1 =
  (pLexeme pInt >>= \i -> return $ IMMI mr1 i)
  <|>
  (pLexeme pId >>= \id -> pEndRHS mr1 (MRefId id))
  <|>
  (pLexeme pStrLit >>= \str -> return $ IMMS mr1 str)
  <|>
  do pReserved "M"
     op <- pBrackets (pTokenB pMRefOrIdx')
     either (\(mr2,i) -> return $ IMRI mr1 mr2 i) (pEndRHS mr1) op
  
pEndRHS :: MRef -> MRef -> Parser Instr
pEndRHS mr1 mr2 = option (IMMM mr1 mr2) (pPartialArith mr1 mr2)

listAOps :: [(String, MRef -> MRef -> MRef -> Instr)]
listAOps = [("+",IAdd), ("-",ISub), ("*",IMul), ("/",IDiv), ("%",IMod)]

pPartialArith :: MRef -> MRef -> Parser Instr
pPartialArith mr1 mr2 =
  choice $ map g listAOps
  where g (s,f) = pReservedOp s
                  >> pMRef2
                  >>= \mr3 -> return $ f mr1 mr2 mr3

pMRefOrIdx' :: Parser (Either (MRef,Int) MRef)
pMRefOrIdx' = do mr <- pMRef2
                 pReservedOp "+"
                 i <- pInteger -- i <- pInt
                 return $ Left (mr,(fromInteger i))
              <|>
              (pInteger >>= \i -> return $ Right (MRefI (fromInteger i)))

-- Helpers parser to identify an integer or identifier
pTknIntOrId :: Parser (Either Int String)
pTknIntOrId = (pLexeme pInt >>= return.Left) <|> (pLexeme pId >>= return.Right)

pInt :: Parser Int
pInt = do l  <- oneOf "123456789"
          ls <- many digit
          return $ (read (l:ls))
      <|> 
       do l <- oneOf "0"
          notFollowedBy digit
          return $ (read (l:[]))

pId :: Parser String
pId = do l <- (letter <|> char '_')
         ls <- many (alphaNum <|> char '_')
         return $ l:ls

-- String Literal Parser
pStrLit :: Parser String
pStrLit = between (pSymbol "\"") (pSymbol "\"") (many $ p)
          <|>
          between (pSymbol "'") (pSymbol "'") (many $ p)
  where p = oneOf " :!#$%&*+./<=>?@\\^|-~()[]" <|> digit <|> letter 
-- Parser candidates to delete, because are deprecated
pToken :: Parser a -> Parser a
pToken p = (p >>= \a -> many (char ' ') >> return a )

pTokenB :: Parser a -> Parser a
pTokenB p = do many (char ' ')
               a <- p
               many (char ' ')
               return a

pSpaces :: Parser ()
pSpaces =  many (char ' ') >> return ()

-- MemRef
pMRef2 :: Parser MRef
pMRef2 = do pReserved "M"
            i<- pBrackets pInteger
            return $ (MRefI (fromInteger i))
         <|>
         (pIdentifier >>= (return.MRefId))
         <?>
         "pMRef2"

-- Equates
pEqu :: Parser Equ
pEqu = do pReserved "equ"
          id <- pIdentifier
          pReserved "M"
          i  <- pBrackets pInteger
          return $ (id,(fromInteger i))

pEquates :: Parser Equates
pEquates = do{ equates <- sepBy pEqu (pWhiteSpace <|> pEOL); eof; return equates }


-- Cond
listCond :: [(String,Cond)]
listCond = [(">",CLT)
           ,(">=",CLET)
           ,("<",CGT)
           ,("<=",CGET)
           ,("=",CE)
           ,("<>",CNE)]

pCond :: Parser Cond
pCond = choice $ map (\(s,r) -> (pReservedOp s >> return r)) listCond

-- End of Line parser
pEOL :: Parser ()
pEOL = (char '\n' >> return ()) <|> (string "\r\n" >> return ())

-- Basic Parsers for compiling assemble files
pOneLineComment :: Parser ()
pOneLineComment = do try (string "#")
                     skipMany (satisfy (/= '\n'))
                     return ()
