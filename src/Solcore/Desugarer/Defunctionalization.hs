module Solcore.Desugarer.Defunctionalization where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map 

import Solcore.Frontend.Syntax 


-- definition of a type to hold lambda abstractions in code 

data LamDef 
  = LamDef { 
      lamArgs :: [Param] -- lambda arguments 
    , lamBody :: Body    -- lambda body 
    } deriving (Eq, Ord, Show)

-- create data types for each lambda abstraction parameter 
-- of a high-order function. 

createDataTy :: Name -> (Name, [LamDef]) -> DataTy 
createDataTy n (f, lams) 
  = DataTy (Name n') [] (map mkConstr lams) 
  where 
    n' = unName n ++ "_" ++ unName f
    mkConstr (LamDef args bd) = undefined

-- determining free variables 

class Vars a where 
  vars :: a -> [Name]

instance Vars a => Vars [a] where 
  vars = foldr (union . vars) []

instance Vars Param where 
  vars (Typed n _) = [n]
  vars (Untyped n) = [n]

instance Vars Stmt where 
  vars (e1 := e2) = vars [e1,e2]
  vars (Let _ _ (Just e)) = vars e
  vars (Let _ _ _) = []
  vars (StmtExp e) = vars e 
  vars (Return e) = vars e 
  vars (Match e eqns) = vars e `union` vars eqns 

instance Vars Equation where 
  vars (_, ss) = vars ss 

instance Vars Exp where 
  vars (Var n) = [n]
  vars (Con _ es) = vars es 
  vars (FieldAccess e _) = vars e
  vars (Call (Just e) _ es) = vars (e : es)
  vars (Call Nothing _ es) = vars es 
  vars (Lam ps bd) = vars bd \\ vars ps
  vars _ = []

-- collecting all lambdas that are parameter of high-order functions 

class CollectLam a where 
  collectLam :: a -> Map Name [LamDef]

instance CollectLam a => CollectLam [a] where 
  collectLam = foldr step Map.empty 
    where 
      step x ac = Map.unionWith (++) (collectLam x) ac

instance CollectLam CompUnit where 
  collectLam (CompUnit _ cs) = collectLam cs

instance CollectLam Contract where 
  collectLam (Contract _ _ decls) = collectLam decls

instance CollectLam Decl where 
  collectLam (ConstrDecl cd) = collectLam cd 
  collectLam (FieldDecl fd) = collectLam fd 
  collectLam (FunDecl fd) = collectLam fd 
  collectLam _ = Map.empty 

instance CollectLam Constructor where 
  collectLam (Constructor _ bd) = collectLam bd 

instance CollectLam Field where 
  collectLam (Field _ _ (Just e)) = collectLam e 
  collectLam _ = Map.empty 

instance CollectLam FunDef where 
  collectLam (FunDef _ bd) = collectLam bd 

instance CollectLam Stmt where 
  collectLam (_ := e) = collectLam e 
  collectLam (Let _ _ (Just e)) = collectLam e
  collectLam (StmtExp e) = collectLam e 
  collectLam (Return e) = collectLam e 
  collectLam (Match es eqns)
    = Map.unionWith (++) (collectLam es) (collectLam eqns)

instance CollectLam Equation where 
  collectLam (_, bd) = collectLam bd 

instance CollectLam Exp where 
  collectLam (Con _ es) = collectLam es 
  collectLam (FieldAccess e _) = collectLam e 
  collectLam (Call (Just e) n es) 
    = Map.unionWith (++) (collectLam e) (collectArgs n es)
  collectLam (Call _ n es) = collectArgs n es 
  collectLam _ = Map.empty 

collectArgs :: Name -> [Exp] -> Map Name [LamDef]
collectArgs n = foldr step Map.empty 
  where 
    step (Lam args bd) ac = Map.insertWith (++) n [LamDef args bd] ac  
    step e ac = Map.unionWith (++) (collectLam e) ac
