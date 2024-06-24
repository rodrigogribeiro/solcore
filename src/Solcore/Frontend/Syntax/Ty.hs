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

infixr 5 :-> 

pattern (:->) a b 
  = TyCon (Name "->") [a, b]

argTy :: Ty -> [Ty]
argTy (t1 :-> t2) = t1 : argTy t2 
argTy _ = []

retTy :: Ty -> Maybe Ty
retTy (TyVar _) = Nothing 
retTy (t1 :-> t2) = ret t2
  where 
    ret (ta :-> tb) = ret tb 
    ret t = Just t

-- definition of constraints 

data Pred = InCls {
              predName :: Name 
            , predMain :: Ty 
            , predParams :: [Ty]
            } 
          | Ty :~: Ty 
          deriving (Eq, Ord, Show)


-- qualified types 

data Qual t 
  = [Pred] :=> t 
    deriving (Eq, Ord, Show)

infix 2 :=> 

-- type schemes 

data Scheme 
  = Forall [Tyvar] (Qual Ty) 
    deriving (Eq, Ord, Show)


