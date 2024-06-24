module Solcore.Frontend.TypeInference.TcUnify where

import Control.Monad.Except

import Data.List

import Solcore.Frontend.Syntax
import Solcore.Frontend.TypeInference.TcMonad
import Solcore.Frontend.TypeInference.TcSubst
import Solcore.Frontend.Pretty.SolcorePretty 


-- standard unification machinery 

varBind :: Tyvar -> Ty -> TcM Subst 
varBind v t
  | v `elem` fv t = infiniteTyErr v t 
  | t == TyVar v = return mempty 
  | otherwise = return (v +-> t)

-- type matching 

match :: Ty -> Ty -> TcM Subst 
match (TyCon n ts) (TyCon n' ts') 
  | n == n' = go ts ts' 
    where 
      go [] [] = pure mempty 
      go (t : ts) (t' : ts') 
        = do 
            sl <- match t t' 
            sr <- go ts ts' 
            merge sl sr
match (TyVar v) t = pure (v +-> t)
match t1 t2 = typesNotMatch t1 t2

matchTypes :: [Ty] -> [Ty] -> TcM Subst 
matchTypes [] [] = pure mempty 
matchTypes (t : ts) (t' : ts') 
  = do 
      s1 <- match t t'
      s2 <- matchTypes ts ts' 
      merge s1 s2 
matchTypes ts ts' = typesMatchListErr ts ts'

-- most general unifier 

mgu :: Ty -> Ty -> TcM Subst 
mgu (TyCon n ts) (TyCon n' ts') 
  | n == n' && length ts == length ts' 
    = solve (zip ts ts') mempty 
mgu (TyVar v) t = varBind v t 
mgu t (TyVar v) = varBind v t 
mgu t1 t2 = typesDoNotUnify t1 t2 

solve :: [(Ty,Ty)] -> Subst -> TcM Subst 
solve [] s = pure s 
solve ((t1, t2) : ts) s 
  = do 
      s1 <- mgu (apply s t1) (apply s t2)
      s2 <- solve ts s1 
      pure (s2 <> s1)

unifyTypes :: [Ty] -> [Ty] -> TcM Subst 
unifyTypes ts ts' = solve (zip ts ts') mempty

unifyAllTypes :: [Ty] -> TcM Subst 
unifyAllTypes [] = pure mempty 
unifyAllTypes (t : ts) 
  = do 
      s1 <- unifyAllTypes ts 
      s2 <- mgu (apply s1 t) (apply s1 t)
      pure (s2 <> s1)

-- composition operator for matching

merge :: Subst -> Subst -> TcM Subst
merge s1@(Subst p1) s2@(Subst p2) = if agree then pure (Subst (p1 ++ p2))
                                             else throwError "merge fails"
  where
    agree = all (\v -> apply s1 (TyVar v) == apply s2 (TyVar v))
                (dom p1 `intersect` dom p2)
    dom s = map fst s

-- basic error messages 

infiniteTyErr :: Tyvar -> Ty -> TcM a 
infiniteTyErr v t 
  = throwError $ unwords ["Cannot construct the infinite type:"
                         , pretty v 
                         , "~"
                         , pretty t
                         ] 

typesNotMatch :: Ty -> Ty -> TcM a 
typesNotMatch t1 t2 
  = throwError $ unwords [ "Types do not match:"
                         , pretty t1 
                         , "and"
                         , pretty t2
                         ]

typesMatchListErr :: [Ty] -> [Ty] -> TcM a 
typesMatchListErr ts ts' 
  = throwError (errMsg (zip ts ts'))
    where 
      errMsg ps = unwords  ["Types do not match:"] ++ 
                           concatMap tyList ps  
      tyList (t1, t2) = pretty t1 <> " and " <> pretty t2

typesDoNotUnify :: Ty -> Ty -> TcM a 
typesDoNotUnify t1 t2 
  = throwError $ unwords [ "Types:"
                         , pretty t1
                         , "and"
                         , pretty t2
                         , "do not unify"
                         ]
