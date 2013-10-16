{
module Language.EWE.Parser where

import Language.EWE.Token(Tkns,Tkn(..))
import Language.EWE.AbsSyn
}

%name pEWE
%tokentype { Tkn }
%error     { parseError }

%token int        { TknInt $$ }
       str        { TknStr $$ }  
       label      { TknLabel $$ }
       id         { TknId $$ }
       ':='       { TknAssgn }
       '('        { TknLPar  }
       ')'        { TknRPar  }
       '['        { TknLBrk  }
       ']'        { TknRBrk  }
       ','        { TknComma }
       '+'        { TknOper '+' }
       '-'        { TknOper '-' }
       '*'        { TknOper '*' }
       '/'        { TknOper '/' }
       '%'        { TknOper '%' }
       '<>'       { TknCond "<>" }
       '='        { TknCond "=" }
       '<'        { TknCond "<" }
       '<='       { TknCond "<=" }
       '>'        { TknCond ">" }
       '>='       { TknCond ">=" }
       'PC'       { TknResWrd "PC" }
       'M'        { TknResWrd "M" }
       'readInt'  { TknResWrd "readInt" }
       'writeInt' { TknResWrd "writeInt" }
       'readStr'  { TknResWrd "readStr" }
       'writeStr' { TknResWrd "writeStr" }
       'goto'     { TknResWrd "goto" }
       if         { TknResWrd "if" }
       'then'     { TknResWrd "then" }
       goto       { TknResWrd "goto" }
       'halt'     { TknResWrd "halt" }
       'break'    { TknResWrd "break" }
       'equ'      { TknResWrd "equ" }
%%

EweProg : Executable Equates { Prg $1 $2 }

Executable : LabelInstr              { [$1] }
           | LabelInstr Executable   { $1:$2 }

LabelInstr : label LabelInstr        { addLabel $1 $2 }
           | Instr                   { Stmt [] $1 }

Instr : MemRef ':=' int                           { IMMI $1 $3  }
      | MemRef ':=' str                           { IMMS $1 $3  }
      | MemRef ':=' 'PC' '+' int                  { IMRPC $1 $5 }
      | 'PC'   ':=' MemRef                        { SPC $3 }
      | MemRef ':=' MemRef                        { IMMM $1 $3 }
      | MemRef ':=' MemRef '+' MemRef             { IAdd $1 $3 $5 }
      | MemRef ':=' MemRef '-' MemRef             { ISub $1 $3 $5 }
      | MemRef ':=' MemRef '*' MemRef             { IMul $1 $3 $5 }
      | MemRef ':=' MemRef '/' MemRef             { IDiv $1 $3 $5 }
      | MemRef ':=' MemRef '%' MemRef             { IMod $1 $3 $5 }
      | MemRef ':=' 'M' '[' MemRef '+' int ']'    { IMRI $1 $5 $7 }
      | 'M' '[' MemRef '+' int ']' ':=' MemRef    { IMMR $3 $5 $8 }
      | 'readInt' '(' MemRef ')'                  { IRI $3 }
      | 'writeInt' '(' MemRef ')'                 { IWI $3 }
      | 'readStr' '(' MemRef ',' MemRef ')'       { IRS $3 $5 }  
      | 'writeStr' '(' MemRef ')'                 { IWS $3 }
      | goto int                                { IGI $2 }
      | goto id                                 { IGS $2 }
      | if MemRef Cond MemRef 'then' goto int { IFI $2 $3 $4 $7 }
      | if MemRef Cond MemRef 'then' goto str { IFS $2 $3 $4 $7 }
      | 'halt'                                    { IH }
      | 'break'                                   { IB }

Equates :                                         { [] }
        | 'equ' id 'M' '[' int ']' Equates        { ($2,$5):$7 }

MemRef : 'M' '[' int ']'                          { MRefI $3 }
       | id                                       { MRefId $1 }

Cond : '>='                                       { CLET }
     | '>'                                        { CLT }
     | '<='                                       { CGET }
     | '<'                                        { CGT }
     | '='                                        { CE }
     | '<>'                                       { CNE }

{

addLabel :: String -> Stmt -> Stmt
addLabel s (Stmt ss i) = Stmt (s:ss) i

parseError :: Tkns -> a
parseError lst = error $ "Parse error: " ++ (show lst) 
}

