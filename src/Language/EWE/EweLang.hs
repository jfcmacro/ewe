module Language.EWE.EweLang(eweLangDef) where

import Text.Parsec
import qualified Language.EWE.Token as Tkn

-- EWE Language definition

eweLangDef :: Tkn.LanguageDef st
eweLangDef = Tkn.LanguageDef {
  Tkn.commentStart = "",
  Tkn.commentEnd   = "",
  Tkn.commentLine  = "#",
  Tkn.nestedComments = False,
  Tkn.identStart     = letter <|> char '_',
  Tkn.identLetter    = alphaNum <|> oneOf "_'",
  Tkn.opStart        = oneOf ":!#$%&*+./<=>?@\\^|-~",
  Tkn.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~",
  Tkn.reservedNames  = ["PC",
                        "M",
                        "writeInt",
                        "writeStr",
                        "readInt",
                        "readStr",
                        "halt",
                        "break",
                        "if",
                        "goto",
                        "then",
                        "equ"],
  Tkn.reservedOpNames = ["+", "-", "*", "/",
                         "%", "<", ">=", "<=",
                         ">", "<>"],
  Tkn.caseSensitive   = False
  }

