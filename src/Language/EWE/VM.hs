
module Language.EWE.VM(runVM,execVM) where

import Language.EWE.AbsSyn
import Language.EWE.Utils(emptyProg, equates, stms)
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Map.Strict as Map
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class(lift)
import Data.Char (ord,chr)
import System.IO(hFlush, stdout,hPutStrLn,hPutStr)
import System.Exit(exitWith,ExitCode(..))

type Memory = Map.Map Int Int

emptyMemory :: Memory
emptyMemory = Map.empty

type PC = Int

initPC :: PC
initPC = 0

data StateVM = StVM { mem :: Memory
                    , prg :: Prog
                    , pc  :: PC
                    , debug :: Bool
                    , steps :: Int
                    } deriving (Show)

emptyStateVM = StVM { mem = emptyMemory
                    , prg = emptyProg
                    , pc  = initPC
                    , debug = False
                    , steps = 0
                    }

type StateVMM = StateT StateVM IO

runVM :: Prog -> IO ()
runVM prg = evalStateT evalVM (initVM prg)

execVM :: Prog -> IO StateVM
execVM prg = execStateT evalVM (initVM prg)

evalVM :: StateVMM ()
evalVM = do
  -- lift $ hPutStrLn stdout "Iniciando maquina"
  evalInstr

initVM :: Prog -> StateVM
initVM p = emptyStateVM { prg = p }

mRef :: MRef -> Equates -> Int
mRef (MRefI i) _  = i
mRef (MRefId s) m = M.fromMaybe (error $ "Internal error: memory reference " ++ s ++ " not found ") $ L.lookup s m

outMem :: Int -> Memory -> (Memory,Int)
outMem r m = M.maybe (Map.insert r 0 m, 0) ((,) m) (Map.lookup r m)

inMem :: Int -> Int -> Memory -> Memory
inMem = Map.insert

incrPC :: StateVM -> Int
incrPC state = (pc state) + 1

ge :: StateVM -> Equates
ge = equates.prg

evalInstr :: StateVMM ()
evalInstr = do
  st <- get
  let pc' = pc st
      prg' = prg st
      (Stmt _ ci)   = stms prg' !! pc'
      st'  = execInstr ci st
  if (hasSideEffects ci)
    then case (ci) of
         (IRI _)   -> do
             i <- lift $ readInt "Enter an integer: "
             let st'' = execIRI i ci st
             put st''
             evalInstr
         (IWI _)   -> do
             let (st'',i) = execIWI ci st
             lift $ putStrLn (show i)
             put st''
             evalInstr
         (IRS _ _) -> do
           s <- lift $ readStr "Enter a string: "
           let st'' = execIRS s ci st
           put st''
           evalInstr
         (IWS _) -> do
             let (st'',s) = execIWS ci st
             lift $ putStrLn (show s)
             put st''
             evalInstr
         (IH)      -> return ()
         (IB)      -> return ()
    else do put st'
            evalInstr

execInstr :: Instr -> StateVM -> StateVM
execInstr (INI)       state =
  state { pc = incrPC state }
execInstr (IMMI mr i) state =
  state { mem = inMem (mRef mr (ge state)) i (mem state)
        , pc  = incrPC state
        }
execInstr (IMMS mr s) st =
  let base  = mRef mr (ge st)
      total = base + length s
      st'   = moveStrInMem s base total st
  in st { mem = mem st'
        , pc = incrPC st
        }
execInstr (IMRPC mr i) state =
  state { mem = inMem (mRef mr (ge state)) ((pc state) + i) (mem state)
        , pc  = incrPC state
        }
execInstr (SPC mr) state =
  state { pc = (mRef mr (ge state))
        }
execInstr (IMMM mr1 mr2) state =
  let m       = mem state
      ge'     = ge state
      (m', v) = outMem (mRef mr2 ge') m
  in state { mem = inMem (mRef mr1 ge') v m'
           , pc  = incrPC state
           }
execInstr (IAdd mrr mra mrb) state = comp (+)   mrr mra mrb state
execInstr (ISub mrr mra mrb) state = comp (-)   mrr mra mrb state
execInstr (IMul mrr mra mrb) state = comp (*)   mrr mra mrb state
execInstr (IDiv mrr mra mrb) state = comp (div) mrr mra mrb state
execInstr (IMod mrr mra mrb) state = comp (mod) mrr mra mrb state
execInstr (IMRI mrr mr  i) state =
  let m          = mem state
      ge'        = ge state
      (m',v)     = outMem (mRef mr ge') m
      (m'', v')  = outMem (v + i) m'
  in state { mem = inMem (mRef mrr ge') v' m''
           , pc  = incrPC state
           }
execInstr (IMMR mri i mr) state =
  let m          = mem state
      ge'        = ge state
      (m', v)    = outMem (mRef mr ge') m
      (m'', v')  = outMem (mRef mri ge') m'
  in state { mem = inMem (v' + i) v m''
           , pc  = incrPC state
           }
execInstr (IGI i) state =
  let prg'  = prg state
      stms' = stms prg'
      l'    = length stms'
  in state { pc = if (i >= 0 && i < l')
                  then i else error "Inst Pos not valid"
           }
execInstr (IGS s) state =
  let prg'  = prg state
      stms' = stms prg'
      i     = lookupLabel s stms'
  in state { pc = i }
execInstr (IFI mra cond mrb i) state =
  let m       = mem state
      pc'     = pc state
      ge'     = ge state
      prg'    = prg state
      stms'   = stms prg'
      l'      = length stms'
      (m',a)  = outMem (mRef mra ge') m
      (m'',b) = outMem (mRef mrb ge') m'
      op      = fun cond
      i'      = if (i >= 0 && i < l')
                then i else error "Inst Pos not valid in condition"
      npc     = if a `op` b then i' else pc' + 1
  in state { mem = m''
           , pc  = npc
           }
execInstr (IFS mra cond mrb s) state =
  let m       = mem state
      pc'     = pc state
      ge'     = ge state
      prg'    = prg state
      stms'   = stms prg'
      l'      = length stms'
      (m',a)  = outMem (mRef mra ge') m
      (m'',b) = outMem (mRef mrb ge') m'
      op      = fun cond
      i       = lookupLabel s stms'
      npc     = if a `op` b then i else pc' + 1
  in state { mem = m''
           , pc  = npc
           }

execIRI :: Int -> Instr -> StateVM -> StateVM
execIRI i (IRI mr) state =
  state { mem = inMem (mRef mr (ge state)) i (mem state)
        , pc  = incrPC state
        }

execIWI :: Instr -> StateVM -> (StateVM, Int)
execIWI (IWI mr ) state =
  let m       = mem state
      ge'     = ge state
      (m', v) = outMem (mRef mr ge') m
  in (state { mem = m', pc  = incrPC state}, v)

execIRS :: String -> Instr -> StateVM -> StateVM
execIRS s (IRS mr1 mr2) state =
  let ge'     = ge state
      m       = mem state
      startP  = mRef mr1 ge'
      lenP    = mRef mr2 ge'
      (m', v) = outMem lenP m
      state'  = state { mem = m' }
      state'' = moveStrInMem s startP (startP + v) state'
  in if v >= 0
     then state'' { pc = incrPC state }
     else error "IRS len is negative"

moveStrInMem :: String -> Int -> Int -> StateVM -> StateVM
moveStrInMem [] st en state
  | st <= en   = moveStrInMem [] (st+1) en
                        (state { mem = inMem st 0 (mem state) })
  | otherwise  = state
moveStrInMem (c:cs) st en state
  | st <= en   = moveStrInMem cs (st+1) en
                        (state { mem = inMem st (ord c) (mem state) })
  | otherwise  = state { mem = inMem st 0 (mem state) }

execIWS :: Instr -> StateVM -> (StateVM, String)
execIWS (IWS mr) state =
  let ge'         = ge state
      start       = mRef mr ge'
      (state', s) = moveStrOutMem start [] state
  in (state' {pc = incrPC state }, s)

moveStrOutMem :: Int -> String -> StateVM -> (StateVM, String)
moveStrOutMem i s state =
  let ge'    = ge state
      m      = mem state
      (m',v) = outMem i m
  in if (v /= 0)
     then moveStrOutMem (i+1) (s++[chr(v)]) (state { mem = m'})
     else (state, s)

lookupLabel :: String -> Stmts -> Int
lookupLabel = lookupLabel' 0

lookupLabel' :: Int -> String -> Stmts -> Int
lookupLabel' n s [] = error "Label not found"
lookupLabel' n s ((Stmt []  _):stms) = lookupLabel' (n+1) s stms
lookupLabel' n s ((Stmt lbls _):stms)
    | s `elem` lbls = n
    | otherwise     = lookupLabel' (n+1) s stms

fun :: Cond -> (Int -> Int -> Bool)
fun CLET = (<=)
fun CLT  = (<)
fun CGET = (>=)
fun CGT  = (>)
fun CE   = (==)
fun CNE  = (/=)

-- comp computes the functions that transform int values
comp :: (Int -> Int -> Int) -> MRef -> MRef -> MRef -> StateVM -> StateVM
comp f mrr mra mrb state =
  let m          = mem state
      ge'        = ge state
      (m', a)    = outMem (mRef mra ge') m
      (m'', b)   = outMem (mRef mrb ge') m'
  in state { mem = inMem (mRef mrr ge') (a `f` b) m''
           , pc  = incrPC state
           }

hasSideEffects :: Instr -> Bool
hasSideEffects (IRI _)   = True
hasSideEffects (IWI _)   = True
hasSideEffects (IRS _ _) = True
hasSideEffects (IWS _)   = True
hasSideEffects (IH)      = True
hasSideEffects (IB)      = True
hasSideEffects _         = False

readInt :: String -> IO Int
readInt msg = do
  putStr (msg ++ "> ")
  hFlush stdout
  readLn

readStr :: String -> IO String
readStr msg = do
  putStr (msg ++ "> ")
  hFlush stdout
  s <- getLine
  -- putStrLn s
  return $ s
