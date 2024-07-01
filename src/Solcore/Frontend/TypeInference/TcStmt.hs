module Solcore.Frontend.TypeInference.TcStmt where

import Control.Monad
import Control.Monad.Except

import Data.List

import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax
import Solcore.Frontend.TypeInference.TcEnv
import Solcore.Frontend.TypeInference.TcMonad
import Solcore.Frontend.TypeInference.TcSubst
import Solcore.Frontend.TypeInference.TcUnify
import Solcore.Primitives.Primitives

-- type inference for statements

tcStmt :: Stmt -> TcM ([Pred], Ty)
tcStmt e@(lhs := rhs) 
  = do 
      (ps1, t1) <- tcExp lhs
      info ["Infered type for ",pretty lhs, " is ", pretty (ps1 :=> t1)]
      (ps2, t2) <- tcExp rhs 
      info ["Infered type for ", pretty rhs, " is ", pretty (ps2 :=> t2)]
      s <- unify t1 t2 `wrapError` e
      extSubst s
      pure (apply s (ps1 ++ ps2), unit)
tcStmt e@(Let n mt me)
  = do 
      (psf, tf) <- case (mt, me) of
                      (Just t, Just e) -> do 
                        (ps1,t1) <- tcExp e 
                        s <- unify t t1 `wrapError` e
                        pure (apply s ps1, apply s t1)
                      (Just t, Nothing) -> do 
                        return ([], t)
                      (Nothing, Just e) -> tcExp e 
                      (Nothing, Nothing) -> 
                        ([],) <$> freshTyVar
      extEnv n (monotype $ stack tf) 
      pure (psf, unit)
tcStmt (StmtExp e)
  = tcExp e 
tcStmt m@(Return e)
  = do 
      (ps, t) <- tcExp e 
      pure (ps, t)
tcStmt (Match es eqns) 
  = do 
      qts <- mapM tcExp es
      resTy <- freshTyVar
      tcEquations qts resTy eqns

tcEquations :: [([Pred], Ty)] -> Ty -> Equations -> TcM ([Pred], Ty)
tcEquations qts resTy eqns 
  = do 
      (ps, ts) <- unzip <$> mapM (tcEquation qts resTy) eqns
      s <- getSubst
      info ["Types infered:", unlines $ map pretty ts]
      return (apply s (concat ps, resTy))

tcEquation :: [([Pred], Ty)] -> Ty -> Equation -> TcM ([Pred], Ty)
tcEquation qts resTy (ps, ss) 
  = do 
      (pss, lctx) <- tcPats qts ps 
      (pss', t) <- withLocalCtx lctx (tcBody ss)
      s <- unify t resTy 
      pure (apply s (pss ++ pss', t))

tcPats :: [([Pred],Ty)] -> [Pat] -> TcM ([Pred], [(Name,Scheme)])
tcPats qts ps 
  | length qts /= length ps = wrongPatternNumber qts ps
  | otherwise = do 
    (pss,ctxs) <- unzip <$> mapM (\(p,t) -> tcPat p t) 
                                 (zip qts ps)
    pure (concat pss, concat ctxs)


tcPat :: ([Pred], Ty) -> Pat -> TcM ([Pred], [(Name, Scheme)])
tcPat (ps, t) p 
  = do 
      (t', pctx) <- tiPat p 
      s <- unify t t' 
      let pctx' = map (\ (n,t) -> (n, monotype $ apply s t)) pctx
      pure (apply s ps, pctx')

tiPat :: Pat -> TcM (Ty, [(Name, Ty)])
tiPat (PVar n) 
  = do 
      t <- freshTyVar
      pure (t, [(n,t)])
tiPat p@(PCon n ps)
  = do
      -- typing parameters 
      (ts, lctxs) <- unzip <$> mapM tiPat ps 
      -- asking type from environment 
      st <- askEnv n
      (ps' :=> tc) <- freshInst st
      tr <- freshTyVar
      s <- unify tc (funtype ts tr) `wrapError` p
      let t' = apply s tr  
      tn <- typeName t'   
      checkConstr tn n 
      let lctx' = map (\(n',t') -> (n', apply s t')) (concat lctxs)
      pure (apply s tr, lctx')
tiPat PWildcard 
  = (, []) <$> freshTyVar
tiPat (PLit l) 
  = do 
      t <- tcLit l 
      pure (t, [])

-- type inference for expressions 

tcLit :: Literal -> TcM Ty 
tcLit (IntLit _) = return word
tcLit (StrLit _) = return string

tcExp :: Exp -> TcM ([Pred], Ty)
tcExp (Lit l) 
  = ([],) <$> tcLit l
tcExp (Var n) 
  = do 
      s <- askEnv n 
      (ps :=> t) <- freshInst s 
      pure (ps, t)
tcExp e@(Con n es)
  = do
      -- typing parameters 
      (pss, ts) <- unzip <$> mapM tcExp es 
      -- getting the type from the environment 
      sch <- askEnv n 
      (ps :=> t) <- freshInst sch
      -- unifying infered parameter types
      t' <- freshTyVar
      s <- unify (funtype ts t') t `wrapError` e 
      tn <- typeName (apply s t')
      -- checking if the constructor belongs to type tn 
      checkConstr tn n
      let ps' = concat (ps : pss)
      pure (apply s (ps', t'))
tcExp (FieldAccess e n) 
  = do
      -- infering expression type 
      (ps,t) <- tcExp e
      -- getting type name 
      tn <- typeName t 
      -- getting field type 
      s <- askField tn n 
      (ps' :=> t') <- freshInst s 
      pure (ps ++ ps', t')
tcExp (Call me n args)
  = tcCall me n args 
tcExp e@(Lam args bd)
  = withLocalSubst do 
      ts' <- mapM addArg args 
      (ps,t') <- tcBody bd 
      s <- getSubst
      let res = apply s (ps, funtype ts' t')
          qt = apply s (ps :=> (funtype ts' t'))
      info ["Infered type for lambda:\n", pretty e, "\n:\n", pretty qt]
      pure res

tcBody :: Body -> TcM ([Pred], Ty)
tcBody [] = pure ([], unit)
tcBody [s] = tcStmt s 
tcBody (s : ss) = tcStmt s >> tcBody ss 

tcCall :: Maybe Exp -> Name -> [Exp] -> TcM ([Pred], Ty)
tcCall Nothing n args 
  = do 
      s <- askEnv n 
      (ps :=> t) <- freshInst s
      t' <- freshTyVar 
      rss <- mapM tcExp args
      s' <- unify t (funtype (map snd rss) t')
      let ps' = foldr (union . fst) [] rss `union` ps
      pure (apply s' ps', apply s' t')
tcCall (Just e) n args 
  = do 
      (ps, ct) <- tcExp e
      s <- askEnv n 
      (ps :=> t) <- freshInst s
      t' <- freshTyVar
      rss <- mapM tcExp args 
      s' <- unify t (funtype (map snd rss) t')
      let ps' = foldr (union . fst) [] rss `union` ps 
      pure (apply s' ps', apply s' t')

addArg :: Param -> TcM Ty 
addArg (Typed n t) 
  = do 
      extEnv n (monotype t)
      pure t 
addArg (Untyped n) 
  = do 
      t <- freshTyVar
      extEnv n (monotype t)
      pure t

tcParam :: Param -> TcM Param
tcParam p@(Typed _ _) = pure p
tcParam (Untyped n) = Typed n <$> freshTyVar

typeName :: Ty -> TcM Name 
typeName (TyCon n _) = pure n
typeName t = throwError $ unlines ["Expected type, but found:"
                                  , pretty t
                                  ]
-- errors 

expectedFunction :: Ty -> TcM a
expectedFunction t 
  = throwError $ unlines ["Expected function type. Found:"
                         , pretty t 
                         ]

wrongPatternNumber :: [([Pred],Ty)] -> [Pat] -> TcM a
wrongPatternNumber qts ps 
  = throwError $ unlines [ "Wrong number of patterns in:"
                         , unwords (map pretty ps)
                         , "expected:"
                         , show (length qts)
                         , "patterns"]
