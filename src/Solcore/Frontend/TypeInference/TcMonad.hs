module Solcore.Frontend.TypeInference.TcMonad where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State 

import Data.Map (Map)
import qualified Data.Map as Map

import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax
import Solcore.Frontend.TypeInference.NameSupply
import Solcore.Frontend.TypeInference.TcSubst


-- definition of type inference monad infrastrature 

type Env = Map Name Scheme
type DataEnv = Map Name Scheme
type TypeEnv = Map Name TypeInfo 

data TypeInfo 
  = TypeInfo {
      fieldEnv :: Map Name Scheme  
    , funEnv :: Map Name Scheme 
    }
    deriving (Eq, Show, Ord)

data TcEnv 
  = TcEnv {
      env :: Env               -- Variable environment
    , constructors :: DataEnv  -- ADT constructor environment
    , typeEnv :: TypeEnv       -- Type environment
    , contract :: Name         -- current contract name 
                               -- used to type check calls.
    , subst :: Subst           -- Current substitution
    , nameSupply :: NameSupply -- Fresh name supply 
    }

type TcM a = StateT TcEnv (ExceptT String Identity) a 

runTcM :: TcM a -> TcEnv -> Either String (a, TcEnv)
runTcM m env = runIdentity (runExceptT (runStateT m env))

freshVar :: TcM Tyvar 
freshVar 
  = do 
      ns <- gets nameSupply
      let (n, ns') = newName ns
      modify (\env -> env {nameSupply = ns'})
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
  = modify (\ env -> env{contract = n})

askCurrentContract :: TcM Name 
askCurrentContract = gets contract

-- environment operations: variables 

maybeAskVar :: Name -> TcM (Maybe Scheme)
maybeAskVar n = gets (Map.lookup n . env)

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
