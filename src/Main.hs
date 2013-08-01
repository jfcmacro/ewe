module Main where

import Language.EWE.Parser
import Language.EWE.AbsSyn
import Language.EWE.VM

main :: IO ()
main = do 
  s    <- getContents
  prog <- pEWE s "Standard Input"
  print prog
