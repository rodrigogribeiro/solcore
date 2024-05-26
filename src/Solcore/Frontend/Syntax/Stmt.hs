module Solcore.Frontend.Syntax.Stmt where 

import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.Ty

-- definition of statements 

data Stmt 
  = Name := Exp             -- assignment
  | Let Name Ty (Maybe Exp) -- local variable  
--  | If Exp Body           -- conditionals 
--  | While Exp Body        -- loops
  | StmtExp Exp             -- expression level statements 
  deriving (Eq, Ord, Show)

type Body = [Stmt]

-- definition of the expression syntax

data Exp 
  = Var Name              -- variable  
  | Con Name [Exp]        -- data type constructor
  | Lit Literal           -- literal 
  | Call Name [Exp]       -- function call 
  | Switch Exp [(Pat, [Stmt])] -- pattern matching 
  deriving (Eq, Ord, Show)

-- pattern matching equations 

data Pat 
  = PVar Name 
  | PCon Name [Pat] 
  | PWildcard 
  | PLit Literal 
  deriving (Eq, Ord, Show)

-- definition of literals 

data Literal 
  = IntLit Integer 
  deriving (Eq, Ord, Show)
