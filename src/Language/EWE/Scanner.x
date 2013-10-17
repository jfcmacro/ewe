{
module Language.EWE.Scanner(Alex(..),
                            AlexPosn(..),
                            alexMonadScan,
                            alexExec,
                            runAlex,
                            alexGetInput,
                            getPosn
                           ) where

import Language.EWE.Token(Tkns,Tkn(..))
}

%wrapper "monad"

$alpha       = [a-zA-Z]
$digit       = [0-9]
$nozerodigit = [1-9]
$graphic     = $printable
$eol         = [\n]

@string = \" (graphic # \")* \"

tokens :-

       $white+                      ;
       \#.*                         ;
       $eol                         { returnEOL }
       \:\=                         { returnAssgn }
       [\*\+\-\/\%]                 { returnOper }
       \(                           { returnLPar}
       \)                           { returnRPar }
       \[                           { returnLBrk }
       \]                           { returnRBrk }
       \,                           { returnComma }
       (\>|\>\=|\<|\<\=|\=|\<\>)    { returnCond }
       "M"                          { returnResWrd }
       "PC"                         { returnResWrd }
       "readInt"                    { returnResWrd }
       "writeInt"                   { returnResWrd }
       "readStr"                    { returnResWrd }
       "writeStr"                   { returnResWrd }
       "goto"                       { returnResWrd }
       "if"                         { returnResWrd }
       "then"                       { returnResWrd }
       "halt"                       { returnResWrd }
       "break"                      { returnResWrd }
       "equ"                        { returnResWrd }
--       \"($alpha $digit)*\"       { returnStr }
       \" [^\"]* \"                 { returnStr }
       (($nozerodigit $digit*)| 0)  { returnInt }
       $alpha [$alpha $digit]*\:    { returnLabel }
       $alpha [$alpha $digit]       { returnId }

{
returnEOL :: AlexInput -> Int -> Alex Tkn
returnEOL = returnTkn $ TknEOL

returnAssgn :: AlexInput -> Int -> Alex Tkn
returnAssgn = returnTkn $ TknAssgn

returnTkn :: Tkn -> AlexInput -> Int -> Alex Tkn
returnTkn tkn _ _ = return $ tkn

returnLPar :: AlexInput -> Int -> Alex Tkn
returnLPar = returnTkn $ TknLPar

returnRPar :: AlexInput -> Int -> Alex Tkn
returnRPar = returnTkn $ TknRPar

returnLBrk :: AlexInput -> Int -> Alex Tkn
returnLBrk = returnTkn $ TknLBrk

returnRBrk :: AlexInput -> Int -> Alex Tkn
returnRBrk = returnTkn $ TknRBrk

returnComma :: AlexInput -> Int -> Alex Tkn
returnComma = returnTkn $ TknComma

returnOper :: AlexInput -> Int -> Alex Tkn
returnOper = returnFunction (TknOper . head)

returnCond :: AlexInput -> Int -> Alex Tkn
returnCond = returnFunction TknCond

returnStr :: AlexInput -> Int -> Alex Tkn
returnStr = returnFunction (TknStr . read)

returnInt :: AlexInput -> Int -> Alex Tkn
returnInt = returnFunction (TknInt . read)

returnLabel :: AlexInput -> Int -> Alex Tkn
returnLabel = returnFunction TknLabel

returnId :: AlexInput -> Int -> Alex Tkn
returnId = returnFunction TknId

returnResWrd :: AlexInput -> Int -> Alex Tkn
returnResWrd = returnFunction TknResWrd

returnFunction :: (String -> Tkn) -> AlexInput -> Int -> Alex Tkn
returnFunction f (_,_,_,s) i = return $ f (take i s)

alexEOF :: Alex Tkn
alexEOF = return $ TknEOF

alexExec :: Alex Tkns
alexExec = do
   t <- alexMonadScan
   case t of
     TknEOF -> return $ [t]
     _      -> do ts <- alexExec
                  return (t:ts)

getPosn :: Alex (Int,Int)
getPosn = do
 (AlexPn _ l c,_,_,_) <- alexGetInput
 return (l,c)
}
