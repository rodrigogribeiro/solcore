module Solcore.Frontend.TypeInference.Constraints where

import Control.Monad.Error.Class
import Data.List(union, intersect, nub, (\\), deleteBy, nubBy, intercalate)
import Text.Show(showListWith) 

import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.Ty

-- definition of constraints 

type Constraint = (Ty, Ty)
type Constraints = [Constraint]

newtype Subst = Subst [(Tyvar, Ty)]

instance Show Subst where 
  showsPrec _ (Subst ps) 
    = showListWith showsPair ps 
      where
        showsPair :: (Tyvar, Ty) -> ShowS
        showsPair (v,t) = showsVar v . showString ":=" . shows t

        showsVar :: Tyvar -> ShowS 
        showsVar (TVar (Name n)) = showString n

emptySubst :: Subst 
emptySubst = Subst []

(+->) :: Tyvar -> Ty -> Subst 
v +-> t = Subst [(v,t)]


expel :: [Tyvar] -> Subst -> Subst
expel [] s = s
expel (v:vs) (Subst s) = Subst $ filter ((v /=) . fst) s

-- definition of substitution application 

class HasType a where 
  apply :: Subst -> a -> a 
  ftv :: a -> [Tyvar]

instance HasType a => HasType [a] where 
  apply s = map (apply s)
  ftv = foldr (union . ftv) []

instance (HasType a, HasType b) => HasType (a, b) where 
  apply s (x, y) = (apply s x, apply s y)
  ftv (x, y) = ftv x `union` ftv y

instance HasType Ty where 
  apply (Subst s) t@(TyVar v) 
    = case lookup v s of 
        Just t' -> t' 
        Nothing -> t 
  apply s (TyCon n ts) 
    = TyCon n (apply s ts)

  ftv (TyVar v) = [v]
  ftv (TyCon _ ts) = ftv ts

instance HasType Pred where 
  apply s (InCls i as t) 
    = InCls i (apply s as) (apply s t)
  apply s (t1 :~: t2) 
    = (apply s t1) :~: (apply s t2)

  ftv (InCls _ as t) = ftv as `union` ftv t 
  ftv (t1 :~: t2) = ftv t1 `union` ftv t2

instance HasType t => HasType (Qual t) where 
  apply s (ps :=> t) = (apply s ps) :=> (apply s t)
  ftv (ps :=> t) = ftv ps `union` ftv t

instance HasType Scheme where 
  apply s (Forall tvs t) = Forall tvs (apply (expel tvs s) t)
  ftv (Forall tvs t) = ftv t \\ tvs


instance Semigroup Subst where
 s1@(Subst ps1) <> s2@(Subst ps2) = Subst (outer ++ inner) where
                         outer = [ (u, apply s1 t) | (u,t) <- ps2 ]
                         inner = [ (v,s) | (v, s) <- ps1, not(elem v dom2)]
                         dom2 = map fst ps2

instance Monoid Subst where
  mempty = emptySubst

merge :: MonadError String m => Subst -> Subst -> m Subst
merge s1@(Subst p1) s2@(Subst p2) = if agree then pure (Subst (p1 ++ p2))
                                             else throwError "merge fails"
  where
    agree = all (\ v -> apply s1 (TyVar v) == apply s2 (TyVar v))
                (dom p1 `intersect` dom p2)
    dom s = map fst s


match   :: MonadError String m => Ty -> Ty -> m Subst
match (TyCon tc1 as1) (TyCon tc2 as2)
  | tc1 == tc2 = matcha as1 as2
  where
    matcha []  [] = pure mempty
    matcha (a:as) (b:bs) = do
      sl <- match a b
      sr <- matcha as bs
      merge sl sr
match (TyVar u) t = pure (u +-> t)
match t u = throwError(unwords["types do not match:", show t, show u])

matchTypes :: MonadError String m => [Ty] -> [Ty] -> m Subst
matchTypes []  [] = pure mempty
matchTypes (a:as) (b:bs) = do
  sl <- match a b
  sr <- matchTypes as bs
  merge sl sr
matchTypes ts us = throwError(unwords["types do not match:", show ts, show us])

varBind :: MonadError String m => Tyvar -> Ty -> m Subst
varBind u t 
  | t == TyVar u     = return emptySubst
  | u `elem` ftv t   = throwError (errInfinite (TyVar u) t)
  | otherwise        = return (u +-> t)

errInfinite :: Ty -> Ty -> String
errInfinite u t = unwords [
  "Cannot construct the infinite type:",
  show u,
  "~",
  show t
  ]

mgu :: MonadError String m => Ty -> Ty -> m Subst
mgu (TyCon n ts1) (TyCon n' ts2) 
  | n == n' && length ts1 == length ts2
    = solve (zip ts1 ts2) mempty
mgu (TyVar u) t        = varBind u t
mgu t (TyVar u)        = varBind u t
mgu t1 t2             = throwError (unwords
                                    ["types",
                                     quote t1,
                                     "and",
                                     quote t2,
                                     "do not unify"])

solve :: MonadError String m => Constraints -> Subst -> m Subst
solve [] subst = pure subst
solve ((l,r):cs) s = do
  s' <- mgu (apply s l) (apply s r)
  s'' <- solve cs s'
  pure (s''<>s')

unifyTypes :: MonadError String m => [Ty] -> [Ty] -> m Subst
unifyTypes ts us = solve (zip ts us) mempty

unifyAllTypes :: MonadError String m => [Ty] -> m Subst
unifyAllTypes [] = pure mempty
unifyAllTypes (t:ts) = do
  s1 <- unifyAllTypes ts
  s2 <- mgu (apply s1 t) (apply s1 t)
  pure (s2 <> s1)

quote :: Show a => a -> String
quote t = "'" ++ show t ++ "'"

mguPred :: Pred -> Pred -> Either String Subst
mguPred (InCls i t as) (InCls i' t' as')
  | i == i' = unifyTypes (t : as) (t' : as')
  | otherwise = throwError "classes differ"

matchPred :: Pred -> Pred -> Either String Subst 
matchPred (InCls i t as) (InCls i' t' as')
  | i == i' = matchTypes (t : as) (t' : as')
  | otherwise = throwError "classes differ"

