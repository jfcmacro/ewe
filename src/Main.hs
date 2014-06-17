module Main where

import Paths_ewe (version)
import System.Environment(getArgs)
import System.IO(openFile, hClose, IOMode(..), hGetContents, stdin, stdout
                ,stderr, hPutStr, hPutStrLn)
import System.Exit(ExitCode(..),exitSuccess)
import Data.Version(Version(..), showVersion)
import System.Exit(exitSuccess)
import System.Console.GetOpt
import Text.PrettyPrint
import Data.Maybe(fromMaybe)
import Control.Monad(when)
import Language.EWE.Token
import Language.EWE.TokenUtils
import Language.EWE.Scanner
import Language.EWE.Parser
import Language.EWE.AbsSyn
import Language.EWE.VM(runVM,execVM)
import Language.EWE.CheckGrammar

data Options =  Options { optShowVersion :: Bool
                        , optShowHelp    :: Bool
                        , optNoExec      :: Bool
                        , optScanOut     :: Bool
                        , optParserOut   :: Bool
                        , optDebug       :: Bool
                        } deriving Show

defaultOptions :: Options
defaultOptions = Options { optShowVersion = False
                         , optShowHelp    = False
                         , optNoExec      = True
                         , optScanOut     = False
                         , optParserOut   = False
                         , optDebug       = False
                         }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['v'] ["version"]
    (NoArg (\opts -> opts { optShowVersion  = True }))
    "shows a version number and quits "
  , Option ['h', '?'] ["help"]
    (NoArg (\opts -> opts { optShowHelp = True }))
    "shows help menu and quits"
  , Option ['n'] ["noexec"]
    (NoArg (\opts -> opts { optNoExec = False }))
    "not execute the current file with ewe-vm"
  , Option ['p'] ["parser"]
    (NoArg (\opts -> opts { optParserOut = True }))
    "shows parser info"
  , Option ['s'] ["scanner"]
    (NoArg (\opts -> opts { optScanOut = True }))
    "shows scan info"
  , Option ['d'] ["debug"]
    (NoArg (\opts -> opts { optDebug = True }))
    "shows debug info"
  ]

compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
  case getOpt Permute options argv of
    (o,n,[]) -> return (foldl (flip id) defaultOptions o,n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: ewe [OPTION...] files..."

showScannerOutput :: Either String Tkns -> IO ()
showScannerOutput scanout =
   case scanout of
      Left err   -> hPutStrLn stderr $ "Scanner error: " ++ show err
      Right tkns -> hPutStrLn stdout $ ppTokens tkns

showParserOutput :: Either String Doc -> IO ()
showParserOutput parseout =
    case parseout of
       Left  msg  ->  hPutStrLn stderr $ show msg
       Right prog ->  hPutStrLn stdout $ render prog

showHelp :: IO ()
showHelp = do
  mapM_ (hPutStrLn stderr) (lines $ usageInfo header options) 
  exitSuccess
  where header = "Usage: ewe [OPTION...] files..."

execProg :: Options -> Bool -> Either String Prog -> IO ()
execProg _    True   _           = return ()
execProg opts False (Right prog) = do
   r <- execVM prog
   return ()
   when (optDebug opts) (hPutStrLn stdout $ show r)
execProg _    False _ = return ()

processFile :: Options -> FilePath -> IO ()
processFile opts fp = do
  when (optDebug opts) (hPutStrLn stdout $ "Processing file: " ++ fp)
  fh   <- openFile fp ReadMode
  s    <- hGetContents fh
  let  scanout = runAlex s alexExec
       pRes   = pEWE s
       errorParser               = either (\_ -> True)  (\_ -> False) pRes
       (passGrammar,errGram,doc) = either (\_ -> (False,[],empty)) checkGrammar  pRes
  when (optScanOut opts)               (showScannerOutput scanout)
  when (optParserOut opts)             (showParserOutput doc)
  when (not passGrammar)               (showErrorGrammar errGram)
  when (optNoExec opts && passGrammar) (execProg opts errorParser pRes)
  hClose fh

showErrorGrammar :: String -> IO ()
showErrorGrammar msg = do
   hPutStrLn stderr $ show msg

checkGrammar :: Prog -> (Bool,String,Doc)
checkGrammar prog = let inh = Inh_Prog
                        syn = wrap_Prog (sem_Prog prog) inh
                    in case (res_Syn_Prog syn) of
                         Left msg -> (False, msg, empty)
                         Right () -> (True, [], pp_Syn_Prog syn)

processStaticOptions :: Options -> IO ()
processStaticOptions opts =
  if optShowVersion opts
  then do hPutStrLn stdout $ "ewe version: " ++ (showVersion version)
          exitSuccess          
  else if optShowHelp opts
       then showHelp
       else return ()

main :: IO ()
main = do
  (opts, fls) <- (getArgs >>= compilerOpts)
  processStaticOptions opts
  mapM_ (processFile opts) fls
  return ()
