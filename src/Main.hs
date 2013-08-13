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

data Options =  Options { optShowVersion :: Bool
                        , optExec :: Bool
                        , optDebug :: Bool
                        } deriving Show

defaultOptions :: Options 
defaultOptions = Options { optShowVersion = False
                         , optExec = False
                         , optDebug = False
                         }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['V','?'] ["version"] 
    (NoArg (\opts -> opts { optShowVersion  = True })) 
    "show version number"
  , Option ['x'] ["exec"] 
    (NoArg (\opts -> opts { optExec = True }))
    "execute the current file with ewe-vm"
  , Option ['d'] ["debug"] 
    (NoArg (\opts -> opts { optDebug = True }))
    "show debug information"
  ]

compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
  case getOpt Permute options argv of
    (o,n,[]) -> return (foldl (flip id) defaultOptions o,n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: ewe [OPTION...] files..."

processFile :: Options -> FilePath -> IO ()
processFile opts fp = do
  hPutStr stdout $ "Processing file: " ++ fp
  fh   <- openFile fp ReadMode
  s    <- hGetContents fh
  let pRes = pEWE s fp
  case pRes of
    Left err   -> hPutStrLn stderr $ " Parsing error at " ++ (show err)
    Right prog -> do hPutStrLn stdout $ " is Ok"
                     if optExec opts 
                       then do hPutStrLn stdout $ show prog
                               r <- execVM prog
                               if (optDebug opts) 
                                then hPutStrLn stdout $ show r
                                else return ()
                       else return ()
  hClose fh

processStaticOptions :: Options -> IO ()
processStaticOptions opts = 
  if optShowVersion opts 
  then hPutStrLn stdout $ "ewe version: " ++ (showVersion version)
  else return ()                     

main :: IO ()
main = do
  (opts, fls) <- (getArgs >>= compilerOpts)
  processStaticOptions opts
  mapM_ (processFile opts) fls
  return ()
