module Language.EWE.TokenUtils(ppTokens) where

import Text.PrettyPrint
import Language.EWE.Token(Tkns,Tkn(..),Tkn_(..))

ppTokens :: Tkns -> String
ppTokens tkns = render $ ppTkns tkns

ppTkns :: Tkns -> Doc
ppTkns = vcat.(map ppTkn)

ppTkn :: Tkn -> Doc
ppTkn (Tkn pos tkn) = ppTkn_ tkn <+> ppPos pos
    where ppPos (l,c) = text "at" <+> text "line:" <+> int l <+>
                        text "col:" <+> int c

ppTkn_ :: Tkn_ -> Doc
ppTkn_ (TknInt i)  = text "Int:"        <+> int i
ppTkn_ (TknStr s)  = text "String:"     <+> text s
ppTkn_ (TknId  s)  = text "Id:"         <+> text s
ppTkn_ (TknAssgn)  = text "Operator:"   <+> text ":="
ppTkn_ (TknColon)  = text "Operator:"   <+> text ":"
ppTkn_ (TknLPar)   = text "Separator:"  <+> text "("
ppTkn_ (TknRPar)   = text "Separator:"  <+> text ")"
ppTkn_ (TknLBrk)   = text "Separator:"  <+> text "["
ppTkn_ (TknRBrk)   = text "Separator:"  <+> text "]"
ppTkn_ (TknOper c) = text "Operator:"   <+> char c
ppTkn_ (TknCond s) = text "Coditional:" <+> text s
ppTkn_ (TknResWrd s) = text "Reserved Word:" <+> text s
ppTkn_ (TknEOL)    = text "<EOL>"
ppTkn_ (TknEOF)    = text "<EOF>"
