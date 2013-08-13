module Language.EWE.AbsSyn(MRef(..)
                          ,Cond(..)
                          ,Equ
                          ,Equates
                          ,Labels
                          ,Stmt(..)
                          ,Stmts
                          ,Prog(..)
                          ,Instr(..)
                          ,emptyProg) where

data AbsSyn = Empty

data MRef = MRefI  Int
          | MRefId String
          deriving (Eq, Show)

data Cond = CLET
          | CLT
          | CGET
          | CGT
          | CE
          | CNE
          deriving (Eq,Show)

type Equ = (String,Int)

type Equates = [Equ]

type Labels = [String]

data Stmt = Stmt Labels Instr
            deriving (Eq,Show)

type Stmts = [Stmt]

data Prog = Prg {stms    :: Stmts
                ,equates :: Equates}
            deriving (Eq,Show)

data Instr = IMMI MRef Int       -- Move Memory Int
           | IMMS MRef String    -- Move Memory String
           | IMRPC MRef Int      -- Move Memory Relative PC
           | SPC  MRef           -- Set PC
           | IMMM MRef MRef      -- Move Memory to Memory
           | IAdd MRef MRef MRef -- Add two Memory References
           | ISub MRef MRef MRef -- Sub two Memory References
           | IMul MRef MRef MRef -- Mul two Memory References
           | IDiv MRef MRef MRef -- Div two Memory References
           | IMod MRef MRef MRef -- Mod two Memory References
           | IMRI MRef MRef Int  -- Move to Memory ref a Indexed Memory Pos
           | IMMR MRef Int  MRef -- Move to Index Memory Ref a Memory Ref
           | IRI  MRef           -- Read an Int and stored into a Mem Ref
           | IWI  MRef           -- Write an Int from a Mem Ref
           | IRS  MRef MRef      -- Read a Str
           | IWS  MRef           -- Write a Str
           | IGI  Int            -- Goto to line
           | IGS  String         -- Goto sym
           | IFI  MRef Cond MRef Int -- If Cond then Int
           | IFS  MRef Cond MRef String --
           | IH                  -- Halt
           | IB                  -- Break
           | INI
            deriving (Eq,Show)

emptyProg :: Prog
emptyProg = Prg { stms = []
                , equates = []
                }
