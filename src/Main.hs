module Main where

import Paths_ewe (version)
import System.Environment(getArgs)
import System.IO(openFile, hClose, IOMode(..), hGetContents, stdin, stdout
                ,stderr, hPutStr, hPutStrLn)
import System.Exit(ExitCode(..),exitSuccess)
import Data.Version(Version(..), showVersion)
import System.Console.GetOpt
import Data.Maybe(fromMaybe)
import Control.Monad(when)
import Language.EWE.Token
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
                        } deriving Show

defaultOptions :: Options
defaultOptions = Options { optShowVersion = False
                         , optShowHelp = False
                         , optNoExec = True
                         , optScanOut  = False
                         , optParserOut = False
                         }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['v'] ["version"]
    (NoArg (\opts -> opts { optShowVersion  = True }))
    "show version number"
  , Option ['h', '?'] ["help"]
    (NoArg (\opts -> opts { optShowHelp = True }))
    "show help menu"
  , Option ['n'] ["noexec"]
    (NoArg (\opts -> opts { optNoExec = False }))
    "execute the current file with ewe-vm"
  , Option ['p'] ["parser"]
    (NoArg (\opts -> opts { optParserOut = True }))
    "show parser info"
  , Option ['s'] ["scanner"]
    (NoArg (\opts -> opts { optScanOut = True }))
    "show scan info"
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
      Right tkns -> hPutStrLn stdout $ show tkns

showParserOutput :: Either String Prog -> IO ()
showParserOutput parseout =
    case parseout of
       Left  msg  ->  hPutStrLn stderr $ show msg
       Right prog ->  hPutStrLn stdout $ show prog

showHelp :: IO ()
showHelp = hPutStrLn stderr $ show (usageInfo header options)
  where header = "Usage: ewe [OPTION...] files..."

execProg :: Bool -> Either String Prog -> IO ()
execProg True _ = return ()
execProg False (Right prog) = do
   r <- execVM prog
   return ()
   -- hPutStrLn stdout $ show r
execProg False _ = return ()

processFile :: Options -> FilePath -> IO ()
processFile opts fp = do
  -- hPutStrLn stdout $ "Processing file: " ++ fp
  fh   <- openFile fp ReadMode
  s    <- hGetContents fh
  let  scanout = runAlex s alexExec
       pRes   = pEWE s
       errorParser           = either (\_ -> True)  (\_ -> False) pRes
       (passGrammar,errGram) = either (\_ -> (False,[])) checkGrammar  pRes
  when (optShowHelp opts)              (showHelp)
  when (optScanOut opts)               (showScannerOutput scanout)
  when (optParserOut opts)             (showParserOutput pRes)
  when (not passGrammar)               (showErrorGrammar errGram)
  when (optNoExec opts && passGrammar) (execProg errorParser pRes)
  hClose fh

showErrorGrammar :: String -> IO ()
showErrorGrammar msg = do
   hPutStrLn stderr $ show msg

checkGrammar :: Prog -> (Bool,String)
checkGrammar prog = let inh = Inh_Prog
                        syn = wrap_Prog (sem_Prog prog) inh
                    in case (res_Syn_Prog syn) of
                         Left msg -> (False, msg)
                         Right () -> (True, [])

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
