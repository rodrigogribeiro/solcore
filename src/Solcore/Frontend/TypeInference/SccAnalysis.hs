module Solcore.Frontend.TypeInference.SccAnalysis where 

import Control.Monad
import Control.Monad.Except
import Control.Monad.Identity 

import Data.Graph.Inductive hiding (mkEdges)
import Data.Graph.Inductive.Query.DFS
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map 

import Solcore.Frontend.Syntax.Contract
import Solcore.Frontend.Syntax.Name 
import Solcore.Frontend.Syntax.Stmt
import Solcore.Frontend.Syntax.Ty 

-- strong connect component analysis for building mutual blocks 

type SCC a = (ExceptT String Identity) a 

sccAnalysis :: CompUnit -> Either String CompUnit 
sccAnalysis m = runIdentity (runExceptT (mkScc m))

mkScc :: CompUnit -> SCC CompUnit
mkScc (CompUnit imps cs) = CompUnit imps <$> mapM sccContract cs

sccContract :: Contract -> SCC Contract
sccContract (Contract n ts ds) 
  = do 
      (cgraph, posMap, declMap) <- mkCallGraph funs 
      newDecls <- rebuildDecls posMap declMap (scc cgraph)
      pure (Contract n ts (newDecls ++ others))
    where 
      isFun (FunDecl _) = True 
      isFun _ = False 
      (funs, others) = partition isFun ds

rebuildDecls :: Map Int Name -> Map Name Decl -> [[Node]] -> SCC [Decl]
rebuildDecls posMap declMap 
  = mapM (rebuildDecl posMap declMap)

rebuildDecl :: Map Int Name -> Map Name Decl -> [Node] -> SCC Decl 
rebuildDecl _ _ [] 
  = throwError "Impossible! Empty node list!"
rebuildDecl pmap dmap [n] 
  = rebuild pmap dmap n 
rebuildDecl pmap dmap ns 
  = MutualDecl <$> mapM (rebuild pmap dmap) ns

rebuild :: Map Int Name -> Map Name Decl -> Node -> SCC Decl 
rebuild pmap dmap n 
  = case Map.lookup n pmap of
      Just k -> 
        case Map.lookup k dmap of
          Just d -> pure d 
          Nothing -> throwError ("Impossible! Undefined decl:" ++ (unName k)) 
      Nothing -> throwError ("Impossible! Undefined decl:" ++ show n)

mkCallGraph :: [Decl] -> SCC (Gr Name (), Map Int Name, Map Name Decl)
mkCallGraph ds 
  = do
      nodes' <- zip [0..] <$> mapM nameOf ds 
      declMap <- mkDeclMap ds
      let
        swap (x,y) = (y,x)
        valMap = Map.fromList (map swap nodes')
      table <- mkCallTable ds 
      edges' <- mkEdges table valMap 
      pure (mkGraph nodes' edges', Map.fromList nodes', declMap)

mkDeclMap :: [Decl] -> SCC (Map Name Decl)
mkDeclMap 
  = foldM step Map.empty  
    where 
      step ac d = (\ n -> Map.insert n d ac) <$> nameOf d  

mkEdges :: Map Name [Name] -> Map Name Int -> SCC [LEdge ()] 
mkEdges table pos 
  = foldM step [] (Map.toList table)
    where 
      step ac (k,vs) = do 
        v' <- findPos k 
        vs' <- mapM findPos vs 
        let ac' = map (\ x -> (v', x, ())) vs' 
        pure (ac' ++ ac)
      findPos k = case Map.lookup k pos of
                    Just n -> pure n 
                    _      -> err k 
      err v = throwError ("Undefined name:\n" ++ (unName v))

mkCallTable :: [Decl] -> SCC (Map Name [Name])
mkCallTable ds 
  = do
      emptyTable <- mkEmptyTable ds
      let funs = Map.keys emptyTable 
      let go (FunDecl fd) ac = Map.insert (funDefName fd) 
                                          (fv (funDefBody fd) `intersect` funs) 
                                          ac  
      pure $ foldr go emptyTable ds  

mkEmptyTable :: [Decl] -> SCC (Map Name [Name])
mkEmptyTable = foldM step Map.empty 
  where 
    step ac d = (\ n -> Map.insert n [] ac) <$> nameOf d

nameOf :: Decl -> SCC Name  
nameOf (FunDecl fd) = pure $ funDefName fd 
nameOf d = throwError ("invalid declaration:" ++ show d)

class FreeVars a where 
  fv :: a -> [Name]

instance FreeVars a => FreeVars [a] where 
  fv = foldr (union . fv) []

instance FreeVars Exp where
  fv (Var n) = [n]
  fv (Con _ es) = fv es 
  fv (FieldAccess e _) = fv e 
  fv (Call (Just e) _ es) = fv (e : es)
  fv _ = []

instance FreeVars Stmt where 
  fv (_ := e) = fv e 
  fv (Let _ _ (Just e)) = fv e 
  fv (StmtExp e) = fv e 
  fv (Return e) = fv e 
  fv (Match es eqns) = fv es `union` fv eqns 

instance FreeVars Equation where 
  fv = fv . snd 
