module Solcore.Desugarer.Defunctionalization where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map 

import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax
import Solcore.Frontend.TypeInference.Id
import Solcore.Frontend.TypeInference.NameSupply 
import Solcore.Frontend.TypeInference.TcSubst 


defunctionalize :: CompUnit Id -> IO ()
defunctionalize cunit  
  = do 
      let ldefs = collectLam cunit 
          n = Name "Lam"
      dt <- mapM (createDataTy n) (Map.toList ldefs)
      mapM_ (putStrLn . pretty) dt 

-- definition of a type to hold lambda abstractions in code 

data LamDef 
  = LamDef { 
      lamArgs :: [Param Id] -- lambda arguments 
    , lamBody :: Body Id    -- lambda body 
    } deriving (Eq, Ord, Show)

-- create data types for each lambda abstraction parameter 
-- of a high-order function. 

createDataTy :: Name -> (Name, [LamDef]) -> IO DataTy 
createDataTy (Name n) ((Name f), lams) 
  = do
      let 
        n' = n ++ "_" ++ f 
        idss = map vars lams
        ids = foldr union [] idss
        tvs = foldr (union . fv . idType) [] ids
      css <- zipWithM (mkConstr n' tvs) idss [0..]
      pure $ DataTy (Name n') tvs css  

mkConstr :: String -> [Tyvar] -> [Id] -> Int -> IO Constr 
mkConstr s tvs ids i  
  = Constr n' <$> mapM (mkConstrParam s tvs . idType) ids 
    where 
      n' = Name (s ++ show i)

mkConstrParam :: String -> [Tyvar] -> Ty -> IO Ty 
mkConstrParam s vs (_ :-> _) 
  = pure $ TyCon (Name s) (TyVar <$> vs)
mkConstrParam _ _ t = pure t 

-- determining free variables 

class Vars a where 
  vars :: a -> [Id]

instance Vars a => Vars [a] where 
  vars = foldr (union . vars) []

instance Vars (Param Id) where 
  vars (Typed n _) = [n]
  vars (Untyped n) = [n]

instance Vars (Stmt Id) where 
  vars (e1 := e2) = vars [e1,e2]
  vars (Let _ _ (Just e)) = vars e
  vars (Let _ _ _) = []
  vars (StmtExp e) = vars e 
  vars (Return e) = vars e 
  vars (Match e eqns) = vars e `union` vars eqns 

instance Vars (Equation Id) where 
  vars (_, ss) = vars ss 

instance Vars (Exp Id) where 
  vars (Var n) = [n]
  vars (Con _ es) = vars es 
  vars (FieldAccess e _) = vars e
  vars (Call (Just e) n es) = vars (e : es)
  vars (Call Nothing n es) = vars es 
  vars (Lam ps bd) = vars bd \\ vars ps
  vars _ = []

instance Vars LamDef where 
  vars (LamDef ps ss) = vars ss \\ vars ps

-- collecting all lambdas that are parameter of high-order functions 

class CollectLam a where 
  collectLam :: a -> Map Name [LamDef]

instance CollectLam a => CollectLam [a] where 
  collectLam = foldr step Map.empty 
    where 
      step x ac = Map.unionWith (++) (collectLam x) ac

instance CollectLam (CompUnit Id) where 
  collectLam (CompUnit _ cs) = collectLam cs

instance CollectLam (Contract Id) where 
  collectLam (Contract _ _ decls) = collectLam decls

instance CollectLam (ContractDecl Id) where 
  collectLam (CFieldDecl fd) = collectLam fd 
  collectLam (CFunDecl fd) = collectLam fd 
  collectLam (CMutualDecl ds) = collectLam ds 
  collectLam (CConstrDecl cs) = collectLam cs 
  collectLam _ = Map.empty 

instance CollectLam (TopDecl Id) where 
  collectLam (TContr cd) = collectLam cd 
  collectLam (TFunDef fd) = collectLam fd 
  collectLam (TInstDef is) = collectLam is 
  collectLam (TMutualDef ts) = collectLam ts 
  collectLam _ = Map.empty 

instance CollectLam (Constructor Id) where 
  collectLam (Constructor _ bd) = collectLam bd 

instance CollectLam (Field Id) where 
  collectLam (Field _ _ (Just e)) = collectLam e 
  collectLam _ = Map.empty 

instance CollectLam (Instance Id) where 
  collectLam (Instance _ _ _ _ fs) 
    = collectLam fs

instance CollectLam (FunDef Id) where 
  collectLam (FunDef _ bd) = collectLam bd 

instance CollectLam (Stmt Id) where 
  collectLam (_ := e) = collectLam e 
  collectLam (Let _ _ (Just e)) = collectLam e
  collectLam (StmtExp e) = collectLam e 
  collectLam (Return e) = collectLam e 
  collectLam (Match es eqns)
    = Map.unionWith (++) (collectLam es) (collectLam eqns)

instance CollectLam (Equation Id) where 
  collectLam (_, bd) = collectLam bd 

instance CollectLam (Exp Id) where 
  collectLam (Con _ es) = collectLam es 
  collectLam (FieldAccess e _) = collectLam e 
  collectLam (Call (Just e) (Id n _) es) 
    = Map.unionWith (++) (collectLam e) 
                         (collectArgs n es)
  collectLam (Call _ (Id n _) es) = collectArgs n es 
  collectLam _ = Map.empty 

collectArgs :: Name -> [Exp Id] -> Map Name [LamDef]
collectArgs n = foldr step Map.empty 
  where 
    step (Lam args bd) ac = Map.insertWith (++) n [LamDef args bd] ac  
    step e ac = Map.unionWith (++) (collectLam e) ac

-- definition of a monad for defunctionalization 

data Env 
  = Env {
      lambdas :: Map Name LamDef -- table containing collect lambdas 
    , nameSupply :: NameSupply -- fresh name supply 
    , functions :: [FunDef Id] -- functions generated
    , contractName :: Maybe Name 
    }

type DefunM a = (StateT Env (ExceptT String Identity)) a 

runDefunM :: Env -> DefunM a -> Either String (a, Env)
runDefunM env m = runIdentity (runExceptT (runStateT m env))

addFunDef :: FunDef Id -> DefunM ()
addFunDef fd 
  = modify (\ env -> env {functions = fd : functions env})

-- initializing the environment

initEnv :: Env 
initEnv = Env Map.empty namePool [] Nothing 
