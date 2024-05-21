module Solcore.Frontend.Syntax.Ty where 

import Solcore.Frontend.Syntax.Name 

-- basic typing infrastructure 

data Tyvar 
  = TVar Name 
  deriving (Eq, Ord, Show)
    

data Ty 
  = TyVar Tyvar      -- type variable 
  | TyCon Name [Ty]  -- type constructor 
  deriving (Eq, Ord, Show)

pattern (+->) a b 
  = TyCon arr [a, b]

-- definition of constraints 

data Pred = Pred {
              predName :: Name 
            , predMain :: Ty 
            , predParams :: [Ty]
            } deriving (Eq, Ord, Show)

-- basic types 

word :: Ty 
word = TyCon "Word" []

arr :: Name  
arr = "->"


