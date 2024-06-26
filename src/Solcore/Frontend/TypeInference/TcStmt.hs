module Solcore.Frontend.TypeInference.TcStmt where

import Control.Monad.Except

import Data.List

import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax
import Solcore.Frontend.TypeInference.TcMonad
import Solcore.Frontend.TypeInference.TcSubst
import Solcore.Frontend.TypeInference.TcUnify
import Solcore.Primitives.Primitives

-- type inference for statements

tcStmt :: Stmt -> TcM [Pred]
tcStmt e@(lhs := rhs) 
  = do 
      (ps1, t1) <- tcExp lhs 
      (ps2, t2) <- tcExp rhs 
      s <- mgu t1 t2 `wrapError` e
      pure (apply s (ps1 ++ ps2))
tcStmt e@(Let n mt me)
  = do 
      (psf, tf) <- case (mt, me) of
                      (Just t, Just e) -> do 
                        (ps1,t1) <- tcExp e 
                        s <- mgu t t1 `wrapError` e
                        pure (apply s ps1, apply s t1)
                      (Just t, Nothing) -> do 
                        return ([], t)
                      (Nothing, Just e) -> tcExp e 
                      (Nothing, Nothing) -> 
                        ([],) <$> freshTyVar
      extEnv n (monotype $ stack tf) 
      pure psf
tcStmt (StmtExp e)
  = fst <$> tcExp e 
tcStmt m@(Return e)
  = do 
      (ps, t) <- tcExp e 
      t' <- askReturnTy 
      s <- mgu t t' `wrapError` m
      pure (apply s ps)
tcStmt (Match es eqns) 
  = do 
      qts <- mapM tcExp es 
      tcEquations qts eqns

tcEquations :: [([Pred], Ty)] -> Equations -> TcM [Pred]
tcEquations qts eqns 
  = concat <$> mapM (tcEquation qts) eqns

tcEquation :: [([Pred], Ty)] -> Equation -> TcM [Pred]
tcEquation qts (ps, ss) 
  = do 
      (pss, lctx) <- tcPats qts ps 
      pss' <- concat <$> withLocalCtx lctx (mapM tcStmt ss)
      pure (pss ++ pss')

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
      s <- mgu t t' 
      let pctx' = map (\ (n,t) -> (n, monotype $ apply s t)) pctx
      pure (apply s ps, pctx')

tiPat :: Pat -> TcM (Ty, [(Name, Ty)])
tiPat (PVar n) 
  = do 
      t <- freshTyVar
      pure (t, [(n,t)])
tiPat (PCon n ps)
  = do 
      (ts, lctxs) <- unzip <$> mapM tiPat ps 
      st <- askCon n
      (ps' :=> tc) <- freshInst st
      tr <- freshTyVar
      s <- mgu tc (funtype ts tr)
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
      s <- askVar n 
      (ps :=> t) <- freshInst s 
      pure (ps, t)
tcExp (Con n es)
  = do 
      s <- askCon n 
      (ps :=> t) <- freshInst s 
      pure (ps, t)
tcExp (FieldAccess e n) 
  = do 
      (ps,t) <- tcExp e
      s <- askField t n 
      (ps' :=> t') <- freshInst s 
      pure (ps ++ ps', t')
tcExp (Call me n args)
  = tcCall me n args 

tcCall :: Maybe Exp -> Name -> [Exp] -> TcM ([Pred], Ty)
tcCall Nothing n args 
  = do 
      cn <- askCurrentContract
      s <- askFun cn n 
      (ps :=> t) <- freshInst s 
      rss <- mapM tcExp args
      s' <- unifyTypes (argTy t) (map snd rss)
      let ps' = foldr (union . fst) [] rss `union` ps
      t' <- returnTy t 
      pure (apply s' ps', apply s' t')
tcCall (Just e) n args 
  = do 
      (ps, ct) <- tcExp e
      tn <- typeName ct 
      s <- askFun tn n 
      (ps :=> t) <- freshInst s 
      rss <- mapM tcExp args 
      s' <- unifyTypes (argTy t) (map snd rss)
      let ps' = foldr (union . fst) [] rss `union` ps 
      t' <- returnTy t 
      pure (apply s' ps', apply s' t')

typeName :: Ty -> TcM Name 
typeName (TyCon n _) = pure n
typeName t = throwError $ unlines ["Expected type, but found:"
                                  , pretty t
                                  ]

returnTy :: Ty -> TcM Ty 
returnTy t 
  = case retTy t of 
      Just t' -> return t' 
      Nothing -> expectedFunction t

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
