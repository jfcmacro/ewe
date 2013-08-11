module Main where

import Paths_ewe (version)
import System.Environment(getArgs)
import System.IO(openFile, hClose, IOMode(..), hGetContents, stdin, stdout
                ,stderr, hPutStr, hPutStrLn)
import System.Exit(ExitCode(..),exitSuccess)
import Data.Version(Version(..), showVersion)
import System.Console.GetOpt
import Data.Maybe(fromMaybe)
import Language.EWE.Parser
import Language.EWE.AbsSyn
import Language.EWE.VM

data Flag = EweVersion deriving Show

options :: [OptDescr Flag]
options =
  [ Option ['V','?'] ["version"] (NoArg EweVersion) "show version number"
  ]

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv =
  case getOpt Permute options argv of
    (o,n,[]) -> return (o,n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: ewe [OPTION...] files..."

processFile :: FilePath -> IO ()
processFile fp = do
  hPutStr stdout $ "Processing file: " ++ fp
  fh   <- openFile fp ReadMode
  s    <- hGetContents fh
  let pRes = pEWE s fp
  case pRes of
    Left err   -> hPutStrLn stderr $ " Parsing error at " ++ (show err)
    Right prog -> hPutStrLn stdout $ " is Ok"
  hClose fh

processFlags :: [Flag] -> IO ()
processFlags = mapM_ processFlag

processFlag :: Flag -> IO ()
processFlag EweVersion = (hPutStrLn stdout $ "ewe version: " ++ (showVersion version)) >> exitSuccess
                         
main :: IO ()
main = do
  (flgs, fls) <- (getArgs >>= compilerOpts)
  processFlags flgs
  mapM_ processFile fls
