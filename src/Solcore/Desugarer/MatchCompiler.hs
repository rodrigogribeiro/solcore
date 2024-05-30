module Solcore.Desugarer.MatchCompiler where

import Control.Monad.Identity 
import Control.Monad.Except
import Control.Monad.Reader 
import Control.Monad.State 
import Control.Monad.Writer 

import Data.List

import Solcore.Frontend.Syntax.Contract
import Solcore.Frontend.Syntax.Stmt 
import Solcore.Frontend.Syntax.Name

-- top level interface for the compiler 

type Equations = [([Pat], [Stmt])]

matchCompiler :: CompUnit -> CompUnit
matchCompiler cunit 
  = undefined -- runCompilerM ns (matchCompilerM es eqns)

-- Compiler monad infra 

type CompilerM a 
  = ReaderT String (ExceptT String 
                   (WriterT [FunDef] 
                   (StateT Int Identity))) a

mkPrefix :: [Name] -> String 
mkPrefix = intercalate "_" . map unName 

inc :: CompilerM Int 
inc = do 
  i <- get 
  put (i + 1)
  return i

freshExpVar :: CompilerM Exp 
freshExpVar 
  = do 
      n <- inc 
      pre <- ask 
      return $ Var $ Name (pre ++ show n)

freshPVar :: CompilerM Pat 
freshPVar 
  = do 
      n <- inc
      pre <- ask 
      return $ PVar $ Name (pre ++ show n)

runCompilerM :: [Name] -> CompilerM a -> (Either String a, [FunDef])
runCompilerM ns m
  = runIdentity $ 
      evalStateT (runWriterT (runExceptT (runReaderT m (mkPrefix ns)))) 0

-- Algorithm main function 

matchCompilerM :: [Exp] -> Equations -> CompilerM [Stmt] 
-- first case: No remaining equations. We generate an error.
matchCompilerM _ [] = matchErrorCase 
-- second case: no scrutinee. Result is the body of the first equation.
matchCompilerM [] ((_, s1) : _) = return s1
matchCompilerM es eqns@(_ : _) 
-- third case: all first patters are variables 
  | allPatsStartsWithVars eqns = thirdCase es eqns
-- fourth case: constructors before variables (if any)
  | hasConstrsBeforeVars eqns = fourthCase es eqns  
  | otherwise = undefined 

-- Implementation of the third case.

thirdCase :: [Exp] -> Equations -> CompilerM [Stmt]
thirdCase _ []   
  = throwError "Panic! Impossible --- thirdCase."
thirdCase (e : es) eqns 
  = do 
      v <- freshExpVar
      let vs = map (pVarToExp . fst) eqns 
          s  = map (\ vi -> (vi, v)) vs 
          eqns' = map (\ (_ : ps, ss) -> (ps, apply s ss)) eqns
          f xs = (v := e) : xs 
      f <$> matchCompilerM es eqns' 

-- Implementation of the fourth case 

fourthCase :: [Exp] -> Equations -> CompilerM [Stmt]
fourthCase _ []
  = throwError "Panic! Impossible --- fourthCase."
fourthCase (e : es) eqns 
  = do 
      let (cons, vars) = span isConstr eqns 
      conEqns <- eqnsForConstrs es cons 
      defEqn <- eqnsForVars es vars 
      return [StmtExp (Match [e] (conEqns ++ defEqn))]

eqnsForConstrs :: [Exp] -> Equations -> CompilerM Equations
eqnsForConstrs es eqns
  = do 
       ps <- firstPats eqns
       pss <- mapM createNewPats ps
       buildNewMatches es pss eqns

buildNewMatches :: [Exp] -> [(Pat,[Pat])] -> Equations -> CompilerM Equations 
buildNewMatches es pss eqns 
  = undefined 

matchPats :: Pat -> Pat -> Bool 
matchPats (PCon n _) (PCon n' _)
  = n == n' 
matchPats (PLit l) (PLit l')
  = l == l'
matchPats _ _ = False 

createNewPats :: Pat -> CompilerM (Pat, [Pat])
createNewPats p@(PCon n ps) = (p,) <$> mapM (const freshPVar) ps
createNewPats p@(PLit l) =  return (p, [])
createNewPats _ = throwError "Panic! Impossible --- createNewPats"

firstPats :: Equations -> CompilerM [Pat]
firstPats 
  = mapM firstPat 
    where 
      firstPat (p : _, _) = return p 
      firstPat _ = throwError "Panic! Impossible --- firstPats"

eqnsForVars :: [Exp] -> Equations -> CompilerM Equations 
eqnsForVars es eqns 
  = do 
      v <- freshPVar
      let 
          v' = pVarToExp [v]
          vs = map (pVarToExp . fst) eqns 
          s = map (\ vi -> (vi, v')) vs 
          eqns' = map (\ (ps, ss) -> (ps, apply s ss)) eqns
      ss' <- matchCompilerM es eqns'
      return [([v], ss')]


hasConstrsBeforeVars :: Equations -> Bool 
hasConstrsBeforeVars 
  = not . null . takeWhile isConstr

isConstr :: ([Pat], [Stmt]) -> Bool 
isConstr ((PCon _ _) : _, _) = True 
isConstr ((PLit _) : _, _) = True
isConstr _ = False

-- this function is safe because every call is 
-- guarded by allPatsStartsWithVars

pVarToExp :: [Pat] -> Exp 
pVarToExp ((PVar n) : _) = Var n 
pVarToExp _ = error "Panic! Impossible --- pVarToExp"

allPatsStartsWithVars :: Equations -> Bool 
allPatsStartsWithVars = all startWithVar
  where 
    startWithVar ((PVar _) : _, _) = True
    startWithVar _ = False 
    
-- substituting variables for pattern compilation

type Subst = [(Exp, Exp)]

class Apply a where 
  apply :: Subst -> a -> a 

instance Apply a => Apply [a] where 
  apply s = map (apply s)

instance (Apply a, Apply b) => Apply (a, b) where
  apply s (a,b) = (apply s a, apply s b)

instance Apply Stmt where 
  apply s (e1 := e2) 
    = e1 := (apply s e2)
  apply s (Let n t me) 
    = Let n t (apply s <$> me)
  apply s (StmtExp e)
    = StmtExp (apply s e)
  apply s (Return e)
    = Return (apply s e)

instance Apply Exp where 
  apply s v@(Var _)
    = maybe v id (lookup v s)
  apply s (Con n es)
    = Con n (apply s es)
  apply s (FieldAccess e n)
    = FieldAccess (apply s e) n
  apply _ e@(Lit _) = e 
  apply s (Call me n es)
    = Call (apply s <$> me) n (apply s es)
  apply s (Match es eqns)
    = Match (apply s es) (apply s eqns) 

instance Apply Pat where 
  apply _ p = p 

-- replacing wildcards by fresh pattern variables 

class ReplaceWildcard a where 
  replace :: a -> CompilerM a 

instance ReplaceWildcard a => ReplaceWildcard [a] where 
  replace = mapM replace

instance ( ReplaceWildcard a
         , ReplaceWildcard b) => ReplaceWildcard (a,b) where 
  replace (a,b) = (,) <$> replace a <*> replace b

instance ReplaceWildcard a => ReplaceWildcard (Maybe a) where 
  replace Nothing  = pure Nothing 
  replace (Just e) = Just <$> replace e 

instance ReplaceWildcard Pat where 
  replace v@(PVar _) = return v 
  replace (PCon n ps)
    = PCon n <$> replace ps
  replace PWildcard
    = freshPVar
  replace p@(PLit _)
    = return p 

instance ReplaceWildcard Exp where 
  replace v@(Var _) = return v 
  replace (Con n es) 
    = Con n <$> replace es 
  replace (FieldAccess e n)
    = (flip FieldAccess n) <$> replace e 
  replace e@(Lit _) = return e 
  replace (Call me n es)
    = Call <$> (replace me) <*> 
               pure n <*> 
               replace es
  replace (Match es eqns)
    = Match <$> replace es <*> replace eqns

instance ReplaceWildcard Stmt where 
  replace = undefined

-- infrastructure for the algorithm 

matchErrorCase :: CompilerM [Stmt]  
matchErrorCase 
  = let e = Call Nothing matchError [errorLit]
    in return $ [StmtExp e]

matchError :: Name 
matchError = Name "revert"

errorLit :: Exp 
errorLit = Lit $ StrLit "Incomplete matching"


