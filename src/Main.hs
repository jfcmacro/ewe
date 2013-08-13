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
import Language.EWE.VM(runVM,execVM)

data Flag = EweVersion | EweExec deriving Show

options :: [OptDescr Flag]
options =
  [ Option ['V','?'] ["version"] (NoArg EweVersion) "show version number"
  , Option ['x'] ["exec"] (NoArg EweExec)
    "execute the current file with ewe-vm"
  ]

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv =
  case getOpt Permute options argv of
    (o,n,[]) -> return (o,n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: ewe [OPTION...] files..."

processFile :: Bool -> FilePath -> IO ()
processFile exec fp = do
  hPutStr stdout $ "Processing file: " ++ fp
  fh   <- openFile fp ReadMode
  s    <- hGetContents fh
  let pRes = pEWE s fp
  case pRes of
    Left err   -> hPutStrLn stderr $ " Parsing error at " ++ (show err)
    Right prog -> do hPutStrLn stdout $ " is Ok"
                     if exec
                       then do hPutStrLn stdout $ show prog
                               r <- execVM prog
                               -- hPutStrLn stdout $ show r
                               return ()
                       else return ()
  hClose fh

processFlags :: [Flag] -> IO Bool
processFlags flags = do
  ls <- mapM processFlag flags
  return $ and ls 

processFlag :: Flag -> IO Bool
processFlag EweVersion = do
  hPutStrLn stdout $ "ewe version: " ++ (showVersion version)
  exitSuccess
  return True
processFlag EweExec = return True

main :: IO ()
main = do
  (flgs, fls) <- (getArgs >>= compilerOpts)
  exec <- processFlags flgs
  mapM_ (processFile exec) fls
  return ()
