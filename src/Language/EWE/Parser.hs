module Language.EWE.Parser where

import System.IO
import Control.Monad
import Text.Parsec
import Text.ParserCombinators.Parsec hiding(try)
import Language.EWE.AbsSyn



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

pComment :: Parser ()
pComment = do char '#'
              many (noneOf ['\n','\r'])
              pEOL
              return ()

pStmts :: Parser Stmts
pStmts = many1 pStmtLine

pLabel :: Parser String
pLabel = do id <- pId
            char ':'
	    return $ id
         <?> "Parsing label"

pStmtLine :: Parser Stmt
pStmtLine = (pComment >> (return $ Stmt [] INI))
            <|>
            do labels <- pLabels
               char '\t'
               instr <- pInstr
               (pComment <|> pEOL)
               return $ Stmt labels instr

pLabels :: Parser [String]
pLabels = pLabel `sepBy` ((many (char ' ') >> return ()) <|> pEOL)


-- Instructions parsers
-- <instr> ::= ...
pInstr :: Parser Instr
pInstr = do instr <- choice [pPrefixMRef
                            ,pPCInstr
                            ,pRead
                            ,pWrite
                            ,pGoto
                            ,pIf
                            ,pOneKw
                            ]
            return instr

-- "PC" ":=" <memref>
-- AbsSyn SPC (Set PC instruction)
pPCInstr :: Parser Instr
pPCInstr = do string "PC"
              pSpaces
              string ":="
              mr <- pMRef2
              return $ SPC mr

-- "readInt" "(" <memref> ")"
-- AbsSyn IRI Instruction Read Int
-- "readStr" "(" <memref> ")"
-- AbsSyn IRS Instruction Read String

pRead :: Parser Instr
pRead = do string "read"
           (string "Int" >> pReadInt) <|> (string "Str" >> pReadStr)

pReadInt :: Parser Instr
pReadInt = pInstr1MRef IRI

pReadStr :: Parser Instr
pReadStr = do pSpaces
              (mr1, mr2) <- pTokenB pReadMRefs
              pSpaces
              return $ IRS mr1 mr2

-- "writeStr" "(" <memref> ")"
-- AbsSyn IWS Instruction Write String
-- "writeInt" "(" <memref> ")"
-- AbsSyn IWI Instruction Write Int
pWrite :: Parser Instr
pWrite = do string "write"
            (string "Int" >> pWriteInt) <|> (string "Str" >> pWriteStr)

pWriteInt :: Parser Instr
pWriteInt = pInstr1MRef IWI

pWriteStr :: Parser Instr
pWriteStr = pInstr1MRef IWS

-- "goto" Integer
-- "goto" Identifier
-- AbsSyn IGI Goto to line
-- AbsSyn IGS Goto to Symbol
pGoto:: Parser Instr
pGoto = do pTokenB $ string "goto"
           iOrId <- pTknIntOrId
           let instr = either IGI IGS iOrId
           return instr

-- "if" <mref> <condition> <memref> "then" "goto" Integer
-- "if" <mref> <condition> <memref> "then" "goto" Identifier
-- AbsSyn IFI If Cond then Int
-- AbsSyn IFS If Cont then Id
pIf :: Parser Instr
pIf = do pTokenB $ string "if"
         mr1 <- pTokenB pMRef2
         cnd <- pTokenB pCond
         mr2 <- pTokenB pMRef2
         pTokenB $ string "then"
         pTokenB $ string "goto"
         iOrId <- pTknIntOrId
         let instr = either (IFI mr1 cnd mr2) (IFS mr1 cnd mr2) iOrId
         return instr

listCond :: [(String,Cond)]
listCond = [(">",CLT)
           ,(">=",CLET)
           ,("<",CGT)
           ,("<=",CGET)
           ,("=",CE)
           ,("<>",CNE)]

pCond :: Parser Cond
pCond = choice $ map (\(s,r) -> (pTokenB (string s) >> return r)) listCond

-- "halt"
-- "break"
-- IH Instruction Halt
-- IB Instruciton Break
listOneKw :: [(String,Instr)]
listOneKw = [("halt", IH), ("break", IB)]

pOneKw :: Parser Instr
pOneKw = choice
         $ map (\(s,r) -> (pTokenB (string s) >> return r)) listOneKw

pReadMRefs :: Parser (MRef,MRef)
pReadMRefs = do between( char '(') (char ')')
                  (do mr1 <- pTokenB pMRef2
                      pTokenB (char ',')
                      mr2 <- pTokenB pMRef2
                      return $ (mr1, mr2))

pInstr1MRef :: (MRef -> Instr) -> Parser Instr
pInstr1MRef f =
  do pSpaces
     mr <- between (char '(')  (char ')') (pTokenB pMRef2)
     return $ f mr

pEOL :: Parser ()
pEOL = (char '\n' >> return ()) <|> (string "\r\n" >> return ())

pPrefixMRef :: Parser Instr
pPrefixMRef =
  do pTokenB $ char 'M'
     op <- between (char '[') (char ']') (pTokenB pMRefOrIdx')
     pTokenB $ string ":="
     either pIMMR pNextRHS op
  <|>
  (pTokenB pId >>= \id -> pTokenB $ string ":=" >> (pTokenB $ pNextRHS (MRefId id)))

pIMMR :: (MRef,Int) -> Parser Instr
pIMMR (mr1,i) =
  do pTokenB $ string ":="
     mr2 <- pTokenB $ pMRef2
     return $ IMMR mr1 i mr2

pNextRHS :: MRef -> Parser Instr
pNextRHS mr1 =
  (pToken pInt >>= \i -> return $ IMMI mr1 i)
  <|>
  (pToken pId >>= \id -> pEndRHS mr1 (MRefId id))
  <|>
  (pToken pStrLit >>= \str -> return $ IMMS mr1 str)
  <|>
  do pToken $ char 'M'
     op <- between (char '[') (char ']') (pTokenB pMRefOrIdx')
     either (\(mr2,i) -> return $ IMRI mr1 mr2 i) (pEndRHS mr1) op
  
pEndRHS :: MRef -> MRef -> Parser Instr
pEndRHS mr1 mr2 = option (IMMM mr1 mr2) (pPartialArith mr1 mr2)

listAOps :: [(String, MRef -> MRef -> MRef -> Instr)]
listAOps = [("+",IAdd), ("-",ISub), ("*",IMul), ("/",IDiv), ("%",IMod)]

pPartialArith :: MRef -> MRef -> Parser Instr
pPartialArith mr1 mr2 =
  choice $ map g listAOps
  where g (s,f) = pTokenB (string s)
                  >> pTokenB pMRef2
                  >>= \mr3 -> return $ f mr1 mr2 mr3

pMRefOrIdx' :: Parser (Either (MRef,Int) MRef)
pMRefOrIdx' = do mr <- pTokenB pMRef2
                 pTokenB (char '+')
                 i <- pTokenB pInt
                 return $ Left (mr,i)
              <|>
              (pTokenB pInt >>= \i -> return $ Right (MRefI i))

pMRef2 :: Parser MRef
pMRef2 = do char 'M'
            pSpaces
            i <- between (char '[') (char ']') (pTokenB pInt)
            pSpaces
            return $ MRefI i
         <|>
         (pTokenB pId >>= \id -> return $ MRefId id)
         <?>
         "pMRef2"

pTknIntOrId :: Parser (Either Int String)
pTknIntOrId = (pTokenB pInt >>= return.Left) <|> (pTokenB pId >>= return.Right)

pInt :: Parser Int
pInt = do l  <- oneOf "123456789"
          ls <- many digit
          return $ (read (l:ls))
      <|> 
       do l <- oneOf "0"
          pSpaces
          return $ (read (l:[]))

pId :: Parser String
pId = do l <- (letter <|> char '_')
         ls <- many (alphaNum <|> char '_')
         return $ l:ls

pStrLit :: Parser String
pStrLit = between (char '"') (char '"') (many $ noneOf "\"")
          <|>
          between (char '\'') (char '\'') (many $ noneOf "'")

pToken :: Parser a -> Parser a
pToken p = (p >>= \a -> many (char ' ') >> return a )

pTokenB :: Parser a -> Parser a
pTokenB p = do many (char ' ')
               a <- p
               many (char ' ')
               return a

pSpaces :: Parser ()
pSpaces =  many (char ' ') >> return ()

pEquates = undefined

-- Basic Parsers for compiling assemble files
pOneLineComment :: Parser ()
pOneLineComment = do try (string "#")
                     skipMany (satisfy (/= '\n'))
                     return ()

pSimpleSpace :: Parser  ()
pSimpleSpace = skipMany1 (satisfy (\c -> c == ' '))

pWhiteSpace :: Parser ()
pWhiteSpace = skipMany (pSimpleSpace <?> "")

pLexeme :: Parser a -> Parser a
pLexeme p = do r <- p
               pWhiteSpace
               return r

-- 
pReservedWords :: [String]
pReservedWords = ["PC"
                 ,"M"
                 ,"readInt"
                 ,"writeInt"
                 ,"readStr"
                 ,"writeStr"
                 ,"goto"
                 ,"if"
                 ,"then"
                 ,"halt"
                 ,"break"
                 ,"equ"]
--

pSymbol :: String -> Parser String
pSymbol name = pLexeme $ string name

pParens :: Parser a -> Parser a
pParens = between (pSymbol "(") (pSymbol ")") 

pBrackets :: Parser a -> Parser a
pBrackets = between (pSymbol "[") (pSymbol "]")

