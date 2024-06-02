module Solcore.Frontend.Syntax.Stmt where 

import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.Ty

-- definition of statements 

type Equation = ([Pat], [Stmt])
type Equations = [Equation]

data Stmt 
  = Exp := Exp                -- assignment
  | Let Name (Maybe Ty) (Maybe Exp) -- local variable  
  | StmtExp Exp               -- expression level statements
  | Return Exp                -- return statements
  | Match [Exp] Equations     -- pattern matching 
  deriving (Eq, Ord, Show)

type Body = [Stmt]

-- definition of the expression syntax

data Exp 
  = Var Name                       -- variable  
  | Con Name [Exp]                 -- data type constructor
  | FieldAccess Exp Name           -- field access  
  | Lit Literal                    -- literal 
  | Call (Maybe Exp) Name [Exp]    -- function call
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
  | StrLit String
  deriving (Eq, Ord, Show)
