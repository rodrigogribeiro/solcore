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

tcStmt :: Stmt -> TcM ()
tcStmt = undefined

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
