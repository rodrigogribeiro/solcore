module Solcore.Desugarer.MatchCompiler where

import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Reader 
import Control.Monad.State 
import Control.Monad.Writer 

import Data.Either 
import Data.List
import qualified Data.List.NonEmpty as L

import Solcore.Desugarer.ReplaceWildcard
import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax
import Solcore.Frontend.TypeInference.Id
import Solcore.Primitives.Primitives

import Text.PrettyPrint.HughesPJ (render, hsep)

{-
 Pattern matching compilation
 ============================

 This module implements the strategy to compile 
 complex pattern matching into simple over just 
 one pattern. Such structure can be used 
 for code generation. 

 We follow the algorithm by Augustsson in: 
https://link.springer.com/content/pdf/10.1007/3-540-15975-4_48.pdf
 - -}


-- top level interface for the compiler 


matchCompiler :: CompUnit Id -> IO (Either String (CompUnit Id))
matchCompiler (CompUnit imps cons) 
  = do
      res <- mapM matchCompilerContract cons 
      case partitionEithers res of 
        ([], cons') -> return $ Right $ CompUnit imps cons'
        (errs, _)   -> return $ Left $ unlines errs

matchCompilerContract :: Contract Id -> IO (Either String (Contract Id))
matchCompilerContract (Contract n ts ds)
  = do 
      res <- runCompilerM [n] (mapM compile ds)
      case res of 
        (Left err, _) -> return $ Left err 
        (Right ds', fs) -> return $ Right $ Contract n ts (ds' ++ map FunDecl fs)

class Compile a where 
  type Res a 
  compile :: a -> CompilerM (Res a)

instance Compile a => Compile [a] where 
  type Res [a] = [Res a]
  compile = mapM compile 

instance Compile a => Compile (Maybe a) where 
  type Res (Maybe a) = Maybe (Res a)
  compile Nothing = return Nothing 
  compile (Just e) = Just <$> compile e

instance Compile (Decl Id) where 
  type Res (Decl Id) = Decl Id
  compile (InstDecl inst)
    = InstDecl <$> compile inst 
  compile (FunDecl fun)
    = FunDecl <$> compile fun 
  compile (ConstrDecl con)
    = ConstrDecl <$> compile con 
  compile d = return d 

instance Compile (Instance Id) where 
  type Res (Instance Id) = Instance Id
  compile (Instance ps n ts m funs)
    = Instance ps n ts m <$> compile funs 

instance Compile (FunDef Id) where 
  type Res (FunDef Id) = FunDef Id
  compile (FunDef sig bd)
    = do
        bd1 <- replace bd
        let n = sigName sig 
        bd' <- local (\ ns -> ns ++ "_" ++ unName n) 
                     (compile bd1)
        return (FunDef sig (concat bd'))

instance Compile (Constructor Id) where 
  type Res (Constructor Id) = Constructor Id
  compile (Constructor ps bd)
    = (Constructor ps . concat) <$> compile bd 

instance Compile (Stmt Id) where 
  type Res (Stmt Id) = [Stmt Id]

  compile (Match es eqns) 
    = do
        v <- matchError 
        let def = [StmtExp $ generateCall v [errorLit]]
        matchCompilerM es def eqns 
  compile s = return [s]

-- Algorithm main function 

matchCompilerM :: [Exp Id] -> [Stmt Id] -> Equations Id -> CompilerM [Stmt Id] 
-- first case: No remaining equations. We return the default body.
matchCompilerM _ d [] = return d
-- second case: no scrutinee. Result is the body of the first equation.
matchCompilerM [] _ ((_, s1) : _) = return s1
matchCompilerM es d eqns@(_ : _) 
-- third case: all first patterns are variables 
  | allPatsStartsWithVars eqns 
      = thirdCase es d eqns
-- fourth case: constructors before variables (if any)
  | hasConstrsBeforeVars eqns 
      = fourthCase es d eqns 
-- fifth case: variable between two sets of constructors
  | hasVarsBetweenConstrs eqns  
      = fifthCase es d eqns 
  | otherwise 
    = error "Panic! Impossible! Shoud not happen!" 

-- Implementation of the third case.

thirdCase :: [Exp Id] -> [Stmt Id] -> Equations Id -> CompilerM [Stmt Id]
thirdCase _ _ []   
  = throwError "Panic! Impossible --- thirdCase."
thirdCase (e : es) d eqns 
  = do 
      x@(Id n t) <- freshId
      let 
          vs = foldr (union . L.head . L.fromList . map vars . fst) [] eqns 
          s  = map (\ vi -> (vi, n)) vs 
          eqns' = map (\ (_ : ps, ss) -> (ps, apply s ss)) eqns
      res <- matchCompilerM es d eqns' 
      return (Let x (Just t) (Just e) : res) 

-- Implementation of the fourth case 

fourthCase :: [Exp Id] -> [Stmt Id] -> Equations Id -> CompilerM [Stmt Id]
fourthCase _ _ []
  = throwError "Panic! Impossible --- fourthCase."
fourthCase (e : es) d eqns 
  = do
      let (cons, vars) = span isConstr eqns
      conEqns <- eqnsForConstrs (e : es) d cons 
      defEqn <- eqnsForVars es d vars 
      return [Match [e] (conEqns ++ defEqn)]

-- implementation of the fifth case 

fifthCase :: [Exp Id] -> [Stmt Id] -> Equations Id -> CompilerM [Stmt Id]
fifthCase [] _ [] 
  = throwError "Panic! Impossible --- fifthCase"
fifthCase es@(_ : _) d eqns@(_ : eqs)
  = do 
      let eqnss = reverse $ splits isConstr eqns
      case unsnoc eqnss of
        Just (eqs, eq) -> do 
          d' <- generateFunctions es d eqs 
          matchCompilerM es d' eq 
        Nothing -> throwError "Panic! Impossible --- fifthCase"

hasVarsBetweenConstrs :: Equations Id -> Bool
hasVarsBetweenConstrs eqns 
  = length (splits isConstr eqns) >= 2 

generateFunctions :: [Exp Id] -> [Stmt Id] -> [Equations Id] -> CompilerM [Stmt Id]
generateFunctions es d [] = return d 
generateFunctions es d (eqn : eqns)
  = do 
      d' <- generateFunction es d eqn 
      generateFunctions es d' eqns 

generateFunction :: [Exp Id] -> [Stmt Id] -> Equations Id -> CompilerM [Stmt Id]
generateFunction es d eqn 
  = do
      n <- newFunName
      ss <- matchCompilerM es d eqn 
      let fd = FunDef (Signature n [] [] Nothing) ss 
      tell [fd]
      v <- (TyVar . TVar) <$> freshName 
      return [StmtExp $ generateCall (Id n v) []] 

newFunName :: CompilerM Name 
newFunName 
  = do 
      n <- inc 
      pre <- ask 
      return (Name $ "fun_" ++ pre ++ "_" ++ show n)

eqnsForConstrs :: [Exp Id] -> [Stmt Id] -> Equations Id -> CompilerM (Equations Id)
eqnsForConstrs es d eqns
  = do
      t <- (TyVar . TVar) <$> freshName
      let def = [StmtExp (Var (Id (Name "default") t))]
      concat <$> mapM (eqForConstr es def) (groupByConstr eqns)

eqForConstr :: [Exp Id] -> [Stmt Id] -> Equations Id -> CompilerM (Equations Id)
eqForConstr es d eqn 
  = mapM (buildEquation es d) eqn  

buildEquation :: [Exp Id] -> [Stmt Id] -> Equation Id -> CompilerM (Equation Id)
buildEquation _ _ ([], _) 
  = throwError "Panic! Impossible --- buildEquation"
buildEquation (_ : es) d (p : ps, ss)
  = do 
        (p', ps', vs) <- instantiatePat p
        ([p'],) <$> matchCompilerM (vs ++ es) d [(ps' ++ ps, ss)] 

instantiatePat :: Pat -> CompilerM (Pat, [Pat], [Exp Id])
instantiatePat p@(PLit _) = return (p, [], [])
instantiatePat (PCon n ps)
  = do
      vs <- mapM (const freshId) ps 
      let vs' = map (\ (Id n _) -> n) vs
      return (PCon n (map PVar vs'), ps, map Var vs)

groupByConstr :: Equations Id -> [Equations Id]
groupByConstr 
  = groupBy conEq . sort 
    where 
      conEq ((PCon n _) : _, _) ((PCon n' _) : _ , _) = n == n' 
      conEq _ _ = False


eqnsForVars :: [Exp Id] -> [Stmt Id] -> Equations Id -> CompilerM (Equations Id)
eqnsForVars es d eqns 
  = do 
      v <- freshPVar
      let 
          [v'] = vars v
          vs  = foldr (union . vars . fst) [] eqns 
          s = map (\ vi -> (vi, v')) vs 
          eqns' = map (\ (ps, ss) -> (ps, apply s ss)) eqns
      ss' <- matchCompilerM es d eqns'
      return [([v], ss')]


hasConstrsBeforeVars :: Equations Id -> Bool 
hasConstrsBeforeVars eqns 
  = let 
       (cs, vs) = span isConstr eqns 
       isVar ((PVar _) : _, _) = True 
       isVar _ = False 
    in (not $ null cs) && all isVar vs 


isConstr :: ([Pat], [Stmt Id]) -> Bool 
isConstr ((PCon _ _) : _, _) = True 
isConstr ((PLit _) : _, _) = True
isConstr _ = False

-- this function is safe because every call is 
-- guarded by allPatsStartsWithVars

allPatsStartsWithVars :: Equations Id -> Bool 
allPatsStartsWithVars = all startWithVar
  where 
    startWithVar ((PVar _) : _, _) = True
    startWithVar _ = False 
    
-- substituting variables for pattern compilation

type Subst = [(Name, Name)]

class Apply a where
  vars :: a -> [Name]
  apply :: Subst -> a -> a 

  vars _ = []

instance Apply a => Apply [a] where 
  apply s = map (apply s)
  vars = foldr (union . vars) []

instance Apply a => Apply (Maybe a) where 
  apply _ Nothing = Nothing 
  apply s (Just x) = Just (apply s x)

instance (Apply a, Apply b) => Apply (a, b) where
  apply s (a,b) = (apply s a, apply s b)

instance Apply (Stmt Id) where 
  apply s (e1 := e2) 
    = e1 := (apply s e2)
  apply s (Let n t me) 
    = Let n t (apply s <$> me)
  apply s (StmtExp e)
    = StmtExp (apply s e)
  apply s (Return e)
    = Return (apply s e)
  apply s (Match es eqns)
    = Match (apply s es) (apply s eqns) 

instance Apply (Exp Id) where 
  apply s v@(Var (Id n t))
    = maybe v f (lookup n s)
      where 
        f v = Var (Id v t)
  apply s (Con n es)
    = Con n (apply s es)
  apply s (FieldAccess e n)
    = FieldAccess (apply s e) n
  apply _ e@(Lit _) = e 
  apply s (Call me n es)
    = Call (apply s me) n (apply s es)
  apply s (Lam args bd) 
    = Lam args (apply s bd)

instance Apply Pat where 
  apply _ p = p

  vars (PVar v) = [v]
  vars (PCon _ ps) = foldr (union . vars) [] ps 
  vars _ = []

-- infrastructure for the algorithm 

matchErrorCase :: CompilerM [Stmt Id]  
matchErrorCase 
  = do
      v <- matchError  
      return [StmtExp $ generateCall v [errorLit]]

generateCall :: Id -> [Exp Id] -> Exp Id
generateCall = Call Nothing

matchError :: CompilerM Id   
matchError 
  = do
      v <- (TyVar . TVar) <$> freshName
      pure (Id (Name "revert") v)

errorLit :: Exp Id  
errorLit = Lit $ StrLit "Incomplete matching"

splits :: (a -> Bool) -> [a] -> [[a]]
splits _ [] = []
splits p xs = (ys ++ ws) : splits p ts 
  where 
    (ys, zs) = span p xs 
    (ws, ts) = span (not . p) zs 
