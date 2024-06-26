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
import Solcore.Frontend.TypeInference.TcSubst
import Solcore.Frontend.TypeInference.TcUnify
import Solcore.Primitives.Primitives


-- definition of type inference monad infrastrature 

type Env = Map Name Scheme
type DataEnv = Map Name Scheme
type TypeEnv = Map Name TypeInfo 
type FieldEnv = Map Name Scheme 
type FunctionEnv = Map Name Scheme 
type Inst = Qual Pred 
type InstEnv = Map Name [Inst] 

data TypeInfo 
  = TypeInfo {
      fieldEnv :: FieldEnv   
    , funEnv :: FunctionEnv
    }
    deriving (Eq, Show, Ord)

data TcEnv 
  = TcEnv {
      ctx :: Env               -- Variable environment
    , constructors :: DataEnv  -- ADT constructor environment
    , typeEnv :: TypeEnv       -- Type environment
    , instEnv :: InstEnv       -- Instance Environment
    , contract :: Name         -- current contract name 
                               -- used to type check calls.
    , returnType :: Ty         -- current function return type.
    , subst :: Subst           -- Current substitution
    , nameSupply :: NameSupply -- Fresh name supply
    , logs :: [String]         -- Logging
    , enableLog :: Bool        -- Enable logging?
    , enableCoverage :: Bool   -- Enable coverage checking?
    }

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
      

-- checking class definitions and adding them to environment 

checkClasses :: [Class] -> TcM ()
checkClasses = mapM_ checkClass 

checkClass :: Class -> TcM ()
checkClass (Class ps n vs v sigs) 
  = mapM_ checkSignature sigs 
    where
      checkSignature sig@(Signature f ctx ps mt)
        | null ctx && v `elem` fv (sigType ps mt) 
          = addClassMethod (InCls n (TyVar v) (TyVar <$> vs))
                           sig 
        | otherwise = 
          throwError $ 
              "invalid class declaration: " ++ (unName n)

addClassMethod :: Pred -> Signature -> TcM ()
addClassMethod p@(InCls _ _ _) (Signature f ctx ps mt) 
  = extFunEnv f (Forall vs ([p] :=> ty))
    where 
      ty = sigType ps mt 
      vs = fv ty 
addClassMethod p@(_ :~: _) (Signature n _ _ _) 
  = throwError $ unlines [
                    "Invalid constraint:"
                  , pretty p 
                  , "in class method:"
                  , unName n
                  ]

sigType :: [Param] -> Maybe Ty -> Ty 
sigType ps Nothing = funtype (map snd ps) unit 
sigType ps (Just t) = funtype (map snd ps) t

-- checking instances and adding them in the environment

checkInstances :: [Instance] -> TcM ()
checkInstances = mapM_ checkInstance 

checkInstance :: Instance -> TcM ()
checkInstance (Instance ctx n ts t funs)
  = do 
      let ipred = InCls n t ts
      insts <- askInstEnv n `wrapError` ipred
      checkOverlap ipred insts
      coverage <- askCoverage
      when coverage (checkCoverage n ts t `wrapError` ipred)
      checkMeasure ctx ipred `wrapError` ipred
      mapM_ (checkMethod ipred) funs 
      return ()

checkOverlap :: Pred -> [Inst] -> TcM ()
checkOverlap _ [] = pure ()
checkOverlap p@(InCls _ t _) (i:is) 
  = do 
        i' <- renameVars (fv t) i
        case i' of 
          (ps :=> (InCls _ t' _)) -> 
            case mgu t t' of
              Right _ -> throwError (unlines ["instance:", pretty p, "with:", pretty i'])
              Left _ -> checkOverlap p is
        return ()

checkCoverage :: Name -> [Ty] -> Ty -> TcM ()
checkCoverage cn ts t 
  = do 
      let strongTvs = fv t 
          weakTvs = fv ts 
          undetermined = weakTvs \\ strongTvs
      unless (null undetermined) $ 
          throwError (unlines [
            "Coverage condition fails for class:"
          , unName cn 
          , "- the type:"
          , pretty t 
          , "does not determine:"
          , intercalate ", " (map pretty undetermined)
          ])

checkMethod :: Pred -> FunDef -> TcM () 
checkMethod p fd = undefined 

-- checking Patterson conditions 

checkMeasure :: [Pred] -> Pred -> TcM ()
checkMeasure ps c 
  = if measure ps < measure c then return () 
    else throwError $ unlines [ "Instance "
                              , pretty c
                              , "does not satisfy the Patterson conditions."]

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
