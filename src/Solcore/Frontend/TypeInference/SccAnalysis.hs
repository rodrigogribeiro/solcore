module Solcore.Frontend.TypeInference.SccAnalysis where 

import Control.Monad
import Control.Monad.Except
import Control.Monad.Identity 
import Control.Monad.Trans

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

type SCC a = (ExceptT String IO) a 

sccAnalysis :: CompUnit Name -> IO (Either String (CompUnit Name))
sccAnalysis m = runExceptT (mkScc m)

mkScc :: CompUnit Name -> SCC (CompUnit Name)
mkScc (CompUnit imps cs) = CompUnit imps <$> mapM sccContract cs

sccContract :: Contract Name -> SCC (Contract Name)
sccContract (Contract n ts ds) 
  = do 
      (cgraph, posMap, declMap) <- mkCallGraph defs
      newDecls <- rebuildDecls posMap declMap (scc cgraph)
      (cgraph', posMap', declMap') <- mkCallGraph newDecls
      newDecls' <- sortDecls posMap' declMap' (topsort cgraph')
      pure (Contract n ts (newDecls' ++ others))
    where 
      isDecl (FunDecl _) = True 
      isDecl _ = False 
      (defs, others) = partition isDecl ds

sortDecls :: Map Int Name -> 
             Map Name (Decl Name) -> 
             [Node] -> 
             SCC [Decl Name]
sortDecls posMap declMap nodes 
  = mapM (rebuild posMap declMap) nodes

rebuildDecls :: Map Int Name -> 
                Map Name (Decl Name) -> 
                [[Node]] -> 
                SCC [Decl Name]
rebuildDecls posMap declMap 
  = mapM (rebuildDecl posMap declMap)

rebuildDecl :: Map Int Name -> 
               Map Name (Decl Name) -> 
               [Node] -> SCC (Decl Name)
rebuildDecl _ _ [] 
  = throwError "Impossible! Empty node list!"
rebuildDecl pmap dmap [n] 
  = rebuild pmap dmap n 
rebuildDecl pmap dmap ns 
  = MutualDecl <$> mapM (rebuild pmap dmap) ns

rebuild :: Map Int Name -> Map Name (Decl Name) -> Node -> SCC (Decl Name)
rebuild pmap dmap n 
  = case Map.lookup n pmap of
      Just k -> 
        case Map.lookup k dmap of
          Just d -> pure d
          Nothing -> throwError ("Impossible! Undefined decl:" ++ (unName k)) 
      Nothing -> throwError ("Impossible! Undefined decl:" ++ show n)

mkCallGraph :: [Decl Name] -> SCC ( Gr Name ()
                                  , Map Int Name
                                  , Map Name (Decl Name))
mkCallGraph ds 
  = do
      let
        nodes' = zip [0..] (concatMap nameOf ds) 
        swap (x,y) = (y,x)
        valMap = Map.fromList (map swap nodes')
        declMap = mkDeclMap ds
      table <- mkCallTable ds
      edges' <- mkEdges table valMap
      pure (mkGraph nodes' edges', Map.fromList nodes', declMap)

mkDeclMap :: [Decl Name] -> Map Name (Decl Name)
mkDeclMap 
  = foldr step Map.empty  
    where 
      step d ac = Map.union ac (Map.fromList (zip (nameOf d) (repeat d)))  

mkEdges :: Map Name [Name] -> Map Name Int -> SCC [LEdge ()] 
mkEdges table pos 
  = foldM step [] (Map.toList table)
    where 
      step ac (k,vs) = do 
        v' <- findPos k 
        vs' <- mapM findPos vs
        let ac' = map (\ x -> (x, v', ())) vs' 
        pure (ac' ++ ac)
      findPos k = case Map.lookup k pos of
                    Just n -> pure n 
                    _      -> err k 
      err v = throwError ("Undefined name:\n" ++ (unName v))

mkCallTable :: [Decl Name] -> SCC (Map Name [Name])
mkCallTable ds 
  = do
      let emptyTable = mkEmptyTable ds
          funs = Map.keys emptyTable 
          names :: Decl Name -> [Name]
          names fd = fv fd
          go d ac = Map.unionWith (++) ac (Map.fromList [(n, names d) | n <- nameOf d]) 
          m = foldr go emptyTable ds
      pure m

mkEmptyTable :: [Decl Name] -> Map Name [Name]
mkEmptyTable = foldr step Map.empty 
  where 
    step d ac = Map.union ac (Map.fromList (zip (nameOf d) (repeat [])))

nameOf :: Decl Name -> [Name]
nameOf (FunDecl fd) = [funDefName fd]
nameOf (MutualDecl ds) = concatMap nameOf ds

funDefName :: FunDef Name -> Name 
funDefName (FunDef sig _) 
  = sigName sig

class FreeVars a where 
  fv :: a -> [Name]

instance FreeVars a => FreeVars [a] where 
  fv = foldr (union . fv) []

instance FreeVars (Exp Name) where
  fv (Con _ es) = fv es 
  fv (FieldAccess e _) = fv e 
  fv (Call _ n es) = n : fv es
  fv _ = []

instance FreeVars (Stmt Name) where 
  fv (_ := e) = fv e 
  fv (Let _ _ (Just e)) = fv e 
  fv (StmtExp e) = fv e 
  fv (Return e) = fv e 
  fv (Match es eqns) = fv es `union` fv eqns 

instance FreeVars (Decl Name) where 
  fv (FunDecl (FunDef sig ss)) = fv ss \\ ps 
    where 
      ps = map f (sigParams sig)
      f (Typed n _) = n 
      f (Untyped n) = n
  fv (MutualDecl ds) = fv ds
  fv _ = []

instance FreeVars (Equation Name) where 
  fv = fv . snd 
