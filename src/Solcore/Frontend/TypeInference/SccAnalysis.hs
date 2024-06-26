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
      (cgraph, posMap, declMap) <- mkCallGraph decls 
      newDecls <- rebuildDecls posMap declMap (scc cgraph)
      pure (Contract n ts (newDecls ++ others))
    where 
      isDecl (FunDecl _) = True 
      isDecl (InstDecl _) = True 
      isDecl _ = False 
      (defs, others) = partition isDecl ds
      fun :: Decl -> [FunDef]
      fun (FunDecl fd) = [fd]
      fun (InstDecl (Instance _ _ _ _ funs)) = funs 
      fun _ = []
      decls = concatMap fun defs

rebuildDecls :: Map Int Name -> Map Name FunDef -> [[Node]] -> SCC [Decl]
rebuildDecls posMap declMap 
  = mapM (rebuildDecl posMap declMap)

rebuildDecl :: Map Int Name -> Map Name FunDef -> [Node] -> SCC Decl
rebuildDecl _ _ [] 
  = throwError "Impossible! Empty node list!"
rebuildDecl pmap dmap [n] 
  = rebuild pmap dmap n 
rebuildDecl pmap dmap ns 
  = MutualDecl <$> mapM (rebuild pmap dmap) ns

rebuild :: Map Int Name -> Map Name FunDef -> Node -> SCC Decl
rebuild pmap dmap n 
  = case Map.lookup n pmap of
      Just k -> 
        case Map.lookup k dmap of
          Just d -> pure (FunDecl d)
          Nothing -> throwError ("Impossible! Undefined decl:" ++ (unName k)) 
      Nothing -> throwError ("Impossible! Undefined decl:" ++ show n)

mkCallGraph :: [FunDef] -> SCC (Gr Name (), Map Int Name, Map Name FunDef)
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

mkDeclMap :: [FunDef] -> SCC (Map Name FunDef)
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

mkCallTable :: [FunDef] -> SCC (Map Name [Name])
mkCallTable ds 
  = do
      emptyTable <- mkEmptyTable ds
      let funs = Map.keys emptyTable 
      let go fd ac = Map.insert (funDefName fd) 
                                (fv (funDefBody fd) `intersect` funs) 
                                ac  
      pure $ foldr go emptyTable ds  

mkEmptyTable :: [FunDef] -> SCC (Map Name [Name])
mkEmptyTable = foldM step Map.empty 
  where 
    step ac d = (\ n -> Map.insert n [] ac) <$> nameOf d

nameOf :: FunDef -> SCC Name  
nameOf fd = pure $ funDefName fd 

funDefName :: FunDef -> Name 
funDefName (FunDef sig _) 
  = sigName sig

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
