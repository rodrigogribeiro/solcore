module Solcore.Frontend.TypeInference.TcSubst where 

import Data.List

import Solcore.Frontend.Syntax

-- basic substitution infrastructure

newtype Subst 
  = Subst { unSubst :: [(Tyvar, Ty)] }

restrict :: Subst -> [Tyvar] -> Subst
restrict (Subst s) vs 
  = Subst [(v,t) | (v,t) <- s, v `notElem` vs]

emptySubst :: Subst
emptySubst = Subst []

-- composition operators

instance Semigroup Subst where 
  s1 <> s2 = Subst (outer ++ inner) 
    where 
      outer = [(u, apply s1 t) | (u, t) <- unSubst s2]
      inner = [(v,t) | (v,t) <- unSubst s1, v `notElem` dom2]
      dom2 = map fst (unSubst s2)

instance Monoid Subst where 
  mempty = emptySubst 

(+->) :: Tyvar -> Ty -> Subst
u +-> t = Subst [(u, t)]

class HasType a where 
  apply :: Subst -> a -> a 
  fv :: a -> [Tyvar]

instance (HasType a, HasType b) => HasType (a,b) where 
  apply s (x,y) = (apply s x, apply s y)
  fv (x,y) = fv x `union` fv y

instance HasType a => HasType [a] where 
  apply s = map (apply s)
  fv = foldr (union . fv) []

instance HasType Ty where
  apply (Subst s) t@(TyVar v)
    = maybe t id (lookup v s)
  apply s (TyCon n ts) 
    = TyCon n (apply s ts)

  fv (TyVar v) = [v]
  fv (TyCon _ ts) = fv ts

instance HasType Pred where 
  apply s (InCls n t ts) = InCls n (apply s t) (apply s ts) 
  apply s (t1 :~: t2) = (apply s t1) :~: (apply s t2)

  fv (InCls _ t ts) = fv (t : ts)
  fv (t1 :~: t2) = fv [t1,t2]

instance HasType a => HasType (Qual a) where 
  apply s (ps :=> t) = (apply s ps) :=> (apply s t)
  fv (ps :=> t) = fv ps `union` fv t 

instance HasType Scheme where 
  apply s (Forall vs t) 
    = Forall vs (apply s' t)   
      where 
        s' = restrict s vs
  fv (Forall vs t)
    = fv t \\ vs
