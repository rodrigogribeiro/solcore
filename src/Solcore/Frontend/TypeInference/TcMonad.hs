module Solcore.Frontend.TypeInference.TcMonad where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State 

import Data.List 
import Data.Map (Map)
import qualified Data.Map as Map

import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax
import Solcore.Frontend.TypeInference.NameSupply
import Solcore.Frontend.TypeInference.TcEnv
import Solcore.Frontend.TypeInference.TcSubst
import Solcore.Frontend.TypeInference.TcUnify
import Solcore.Primitives.Primitives


-- definition of type inference monad infrastructure 

type TcM a = StateT TcEnv (ExceptT String Identity) a 

runTcM :: TcM a -> TcEnv -> Either String (a, TcEnv)
runTcM m env = runIdentity (runExceptT (runStateT m env))

freshVar :: TcM Tyvar 
freshVar 
  = do 
      ns <- gets nameSupply
      let (n, ns') = newName ns
      modify (\ ctx -> ctx {nameSupply = ns'})
      return (TVar n)

freshTyVar :: TcM Ty 
freshTyVar = TyVar <$> freshVar

-- type instantiation 

freshInst :: Scheme -> TcM (Qual Ty)
freshInst (Forall vs qt)
  = renameVars vs qt

renameVars :: HasType a => [Tyvar] -> a -> TcM a 
renameVars vs t 
  = do 
      s <- mapM (\ v -> (v,) <$> freshTyVar) vs 
      pure $ apply (Subst s) t

-- current contract manipulation 

setCurrentContract :: Name -> TcM ()
setCurrentContract n 
  = modify (\ ctx -> ctx{contract = n})

askCurrentContract :: TcM Name 
askCurrentContract = gets contract

-- current function return type 

setReturnTy :: Ty -> TcM ()
setReturnTy t 
  = modify (\ ctx -> ctx {returnType = t})

askReturnTy :: TcM Ty 
askReturnTy = gets returnType

-- extending the environment with a new variable 

extEnv :: Name -> Scheme -> TcM ()
extEnv n t 
  = modify (\ sig -> sig {ctx = Map.insert n t (ctx sig)})

withExtEnv :: Name -> Scheme -> TcM a -> TcM a 
withExtEnv n s m 
  = withLocalEnv (extEnv n s >> m)

withLocalCtx :: [(Name, Scheme)] -> TcM a -> TcM a 
withLocalCtx ctx m 
  = withLocalEnv do 
        mapM_ (\ (n,s) -> extEnv n s) ctx 
        a <- m
        pure a

-- Updating the environment 

putEnv :: Env -> TcM ()
putEnv ctx 
  = modify (\ sig -> sig{ctx = ctx})

-- Extending the environment 

withLocalEnv :: TcM a -> TcM a 
withLocalEnv ta 
  = do 
      ctx <- gets ctx 
      a <- ta 
      putEnv ctx 
      pure a 

-- environment operations: variables 

maybeAskVar :: Name -> TcM (Maybe Scheme)
maybeAskVar n = gets (Map.lookup n . ctx)

askVar :: Name -> TcM Scheme 
askVar n 
  = do 
      s <- maybeAskVar n
      maybe (undefinedName n) pure s

-- constructors

maybeAskCon :: Name -> TcM (Maybe Scheme)
maybeAskCon n = gets (Map.lookup n . constructors)

askCon :: Name -> TcM Scheme
askCon n 
  = do 
      t <- maybeAskCon n 
      maybe (undefinedName n) pure t

-- type information

maybeAskTypeInfo :: Name -> TcM (Maybe TypeInfo)
maybeAskTypeInfo n 
  = gets (Map.lookup n . typeEnv)

askTypeInfo :: Name -> TcM TypeInfo 
askTypeInfo n 
  = do 
      ti <- maybeAskTypeInfo n 
      maybe (undefinedType n) pure ti

modifyTypeInfo :: Name -> TypeInfo -> TcM ()
modifyTypeInfo n ti 
  = do 
        tenv <- gets typeEnv
        let tenv' = Map.insert n ti tenv 
        modify (\env -> env{typeEnv = tenv'})

-- field information 

maybeAskField :: Name -> Name -> TcM (Maybe Scheme)
maybeAskField t n 
  = do 
      ti <- askTypeInfo t 
      pure $ Map.lookup n (fieldEnv ti)

askField :: Ty -> Name -> TcM Scheme
askField (TyCon n _) n' 
  = do 
      s <- maybeAskField n n' 
      maybe (undefinedField n' n) pure s
askField t _
  = throwError $ unwords ["Invalid type constructor:", pretty t]

-- function information 

maybeAskFun :: Name -> Name -> TcM (Maybe Scheme)
maybeAskFun t n 
  = do 
      ti <- askTypeInfo t 
      pure $ Map.lookup n (funEnv ti)

askFun :: Name -> Name -> TcM Scheme 
askFun t n 
  = do 
      s <- maybeAskFun t n 
      maybe (undefinedFunction t n) pure s 

extFunEnv :: Name -> Scheme -> TcM ()
extFunEnv n s 
  = do 
      cn <- askCurrentContract 
      ti <- askTypeInfo cn
      let ti' = ti{funEnv = Map.insert n s (funEnv ti)}
      modifyTypeInfo cn ti'

-- manipulating the instance environment 

askInstEnv :: Name -> TcM [Inst]
askInstEnv n 
  = maybe [] id . Map.lookup n <$> gets instEnv

addInstance :: Name -> Inst -> TcM ()
addInstance n inst 
  = modify (\ ctx -> 
      ctx{instEnv = Map.insertWith (++) n [inst] (instEnv ctx)})  
      

maybeToTcM :: String -> Maybe a -> TcM a 
maybeToTcM s Nothing = throwError s 
maybetoTcM _ (Just x) = pure x

-- checking coverage pragma 

askCoverage :: TcM Bool 
askCoverage = gets enableCoverage

setCoverage :: Bool -> TcM ()
setCoverage b 
  = modify (\env -> env{ enableCoverage = b})

-- logging utilities

setLogging :: Bool -> TcM ()
setLogging b = modify (\ r -> r{enableLog = b})

isLogging :: TcM Bool 
isLogging = gets enableLog

info :: [String] -> TcM ()
info ss = do
            logging <- isLogging
            when logging $ modify (\ r -> r{ logs = concat ss : logs r })

warn :: [String] -> TcM ()
warn ss 
  = modify (\ r -> r{ logs = concat ss : logs r })

-- wrapping error messages 

wrapError :: Pretty b => TcM a -> b -> TcM a
wrapError m e 
  = catchError m handler 
    where 
      handler msg = throwError (decorate msg)
      decorate msg = msg ++ "\n - in:" ++ pretty e

-- error messages 

undefinedName :: Name -> TcM a 
undefinedName n 
  = throwError $ unwords ["Undefined name:", pretty n]

undefinedType :: Name -> TcM a 
undefinedType n 
  = throwError $ unwords ["Undefined type:", pretty n]

undefinedField :: Name -> Name -> TcM a 
undefinedField n n'
  = throwError $ unlines ["Undefined field:"
                         , pretty n
                         , "in type:"
                         , pretty n'
                         ]

undefinedFunction :: Name -> Name -> TcM a 
undefinedFunction t n 
  = throwError $ unlines [ "The type:"
                         , pretty t 
                         , "does not define function:"
                         , pretty n
                         ]
