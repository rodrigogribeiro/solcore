module Solcore.Frontend.Syntax.Ty where 

import Solcore.Frontend.Syntax.Name 

-- basic typing infrastructure 

data Tyvar 
  = TVar { unVar :: Name }  
  deriving (Eq, Ord, Show)
    

data Ty 
  = TyVar Tyvar      -- type variable 
  | TyCon Name [Ty]  -- type constructor
  deriving (Eq, Ord, Show)

infixr 5 :->
pattern (:->) a b 
  = TyCon "->" [a,b]

pattern TInt 
  = TyCon "Int" []

type Class = Name 
type Inst = Qual Pred


unitT :: Ty 
unitT = TyCon "Unit" []

stackT :: Ty -> Ty 
stackT t = TyCon "Stack" [t]

argTypes :: Ty -> [Ty]
argTypes (a :-> b) = a : argTypes b 
argTypes _ = []

monotype :: Ty -> Scheme
monotype t = Forall [] ([] :=> t)

forAll :: String -> Ty -> Scheme
forAll s t = Forall (map (TVar . Name) (words s)) ([] :=> t)

-- definition of constraints 

data Pred = InCls {
              predName :: Name 
            , predMain :: Ty 
            , predParams :: [Ty]
            }
          | Ty :~: Ty 
          deriving (Eq, Ord, Show)

pattern IsIn c t = InCls c t []

-- qualified types 

data Qual t = [Pred] :=> t 
  deriving (Eq, Ord, Show)

-- type schemes 

data Scheme = Forall [Tyvar] (Qual Ty) 
      deriving (Eq, Ord, Show)

-- type measure for Patterson conditions 

class HasMeasure a where 
  measure :: a -> Int 

instance HasMeasure a => HasMeasure [a] where 
  measure = sum . map measure 

instance HasMeasure Ty where 
  measure (TyVar _) = 1
  measure (TyCon _ ts) = 1 + measure ts 

instance HasMeasure Pred where 
  measure (InCls _ t ts) = measure (t : ts)
  measure (t1 :~: t2) = measure t1 + measure t2

-- basic types 

word :: Ty 
word = TyCon "Word" []

arr :: Name  
arr = "->"


