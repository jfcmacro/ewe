{
module Language.EWE.Scanner where

import Language.EWE.Token
}

%wrapper "posn"

$alpha       = [a-zA-Z]
$digit       = [0-9]
$nozerodigit = [1-9]
$graphic     = $printable

@string = \" (graphic # \")* \"
@eol    = (\n|\r\n)

tokens :-

       $white+                      ;
       \t                           ;
       @eol                         { \s -> TknEOL }
       M                            { \s -> TknResWrd s}
       PC                           { \s -> TknRewWrd s}
       readInt                      { \s -> TknRewWrd s}
       writeInt                     { \s -> TknRewWrd s}
       readStr                      { \s -> TknRewWrd s}
       writeStr                     { \s -> TknRewWrd s}
       goto                         { \s -> TknRewWrd s}
       if                           { \s -> TknRewWrd s}
       then                         { \s -> TknRewWrd s}
       halt                         { \s -> TknRewWrd s}
       break                        { \s -> TknRewWrd s}
       \:\=                         { \s -> TknAssgn }
       equ                          { \s -> TknRewWrd s}
       [\*\+\-\/\%]                 { \s -> TknOper (head s)}
       \(                           { \s -> TknLPar}
       \)                           { \s -> TknRPar }
       \[                           { \s -> TknLBrk }
       \]                           { \s -> TknRBrk }
       \,                           { \s -> TknComma }
       (\>|\>\=|\<|\<\=|\=|\<\>)    { \s -> TknCond s}
       \"($alpha $digit)*\"         { \s -> TknStr s }
       (($nozerodigit $digit*)| 0)  { \s -> TknInt (read s) }
       $alpha [$alpha $digit]*\:    { \s -> TknLabel s }
       $alpha [$alpha $digit]       { \s -> TknId s } 
