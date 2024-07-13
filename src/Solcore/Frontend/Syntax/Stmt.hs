module Solcore.Frontend.Syntax.Stmt where 

import Data.Generics (Data, Typeable)

import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.Ty

-- definition of statements 

type Equation a = ([Pat], [Stmt a])
type Equations a = [Equation a]

data Stmt a
  = (Exp a) := (Exp a)                  -- assignment
  | Let a (Maybe Ty) (Maybe (Exp a))    -- local variable  
  | StmtExp (Exp a)                     -- expression level statements
  | Return (Exp a)                      -- return statements
  | Match [Exp a] (Equations a)         -- pattern matching
  | Asm YulBlock                        -- Yul block 
  deriving (Eq, Ord, Show, Data, Typeable)

type Body a = [Stmt a]
type YulBlock = [YulStmt]

data YulStmt 
  = YAssign [Name] YulExp
  | YBlock YulBlock
  | YLet [Name] YulExp 
  | YExp YulExp 
  | YIf YulExp YulBlock 
  | YSwitch YulExp YulCases YulDefault
  | YFor YulBlock YulExp YulBlock YulBlock 
  | YContinue 
  | YBreak 
  | YLeave 
  deriving (Eq, Ord, Show, Data, Typeable)

type YulCases = [(Literal, YulBlock)] 
type YulDefault = Maybe (YulBlock)

data YulExp 
  = YLit Literal 
  | YIdent Name 
  | YCall Name [YulExp]
  deriving (Eq, Ord, Show, Data, Typeable)

data Param a 
  = Typed a Ty 
  | Untyped a 
  deriving (Eq, Ord, Show, Data, Typeable)

-- definition of the expression syntax

data Exp a 
  = Var a                              -- variable  
  | Con a [Exp a]                      -- data type constructor
  | FieldAccess (Exp a) a              -- field access  
  | Lit Literal                        -- literal 
  | Call (Maybe (Exp a)) a [Exp a]     -- function call
  | Lam [Param a] (Body a)               -- lambda-abstraction
  deriving (Eq, Ord, Show, Data, Typeable)

-- pattern matching equations 

data Pat 
  = PVar Name 
  | PCon Name [Pat] 
  | PWildcard 
  | PLit Literal 
  deriving (Eq, Ord, Show, Data, Typeable)

-- definition of literals 

data Literal 
  = IntLit Integer
  | StrLit String
  deriving (Eq, Ord, Show, Data, Typeable)
