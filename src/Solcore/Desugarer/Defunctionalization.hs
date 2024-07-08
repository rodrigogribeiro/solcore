module Solcore.Desugarer.Defunctionalization where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map 

import Solcore.Frontend.Syntax 
import Solcore.Frontend.TypeInference.Id
import Solcore.Frontend.TypeInference.TcSubst 

-- definition of a type to hold lambda abstractions in code 

data LamDef 
  = LamDef { 
      lamArgs :: [Param Id] -- lambda arguments 
    , lamBody :: Body Id    -- lambda body 
    } deriving (Eq, Ord, Show)

-- create data types for each lambda abstraction parameter 
-- of a high-order function. 

createDataTy :: Name -> (Name, [LamDef]) -> DataTy 
createDataTy (Name n) ((Name f), lams) 
  = DataTy (Name n') tvs (zipWith (mkConstr n' tvs) idss [0..]) 
    where
      n' = n ++ "_" ++ f 
      idss = map vars lams
      ids = foldr union [] idss
      tvs = foldr (union . fv . idType) [] ids

mkConstr :: String -> [Tyvar] -> [Id] -> Int -> Constr 
mkConstr s tvs ids i  
  = Constr (Name (s ++ show i)) 
           (map (mkConstrParam s tvs . idType) ids)

mkConstrParam :: String -> [Tyvar] -> Ty -> Ty 
mkConstrParam s vs (_ :-> _) = TyCon (Name s) (TyVar <$> vs)
mkConstrParam _ _ t = t 

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
  vars (Call (Just e) _ es) = vars (e : es)
  vars (Call Nothing _ es) = vars es 
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

instance CollectLam (Decl Id) where 
  collectLam (ConstrDecl cd) = collectLam cd 
  collectLam (FieldDecl fd) = collectLam fd 
  collectLam (FunDecl fd) = collectLam fd 
  collectLam _ = Map.empty 

instance CollectLam (Constructor Id) where 
  collectLam (Constructor _ bd) = collectLam bd 

instance CollectLam (Field Id) where 
  collectLam (Field _ _ (Just e)) = collectLam e 
  collectLam _ = Map.empty 

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
  collectLam (Call (Just e) n es) 
    = Map.unionWith (++) (collectLam e) (collectArgs n es)
  collectLam (Call _ n es) = collectArgs n es 
  collectLam _ = Map.empty 

collectArgs :: Name -> [Exp Id] -> Map Name [LamDef]
collectArgs n = foldr step Map.empty 
  where 
    step (Lam args bd) ac = Map.insertWith (++) n [LamDef args bd] ac  
    step e ac = Map.unionWith (++) (collectLam e) ac
