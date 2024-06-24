module Solcore.Desugarer.InternalSyntaxCompiler where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.DFS

import Data.List

import Data.Map (Map)
import qualified Data.Map as Map 

import Solcore.Frontend.Syntax.Contract
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.Stmt
import Solcore.Frontend.Syntax.Ty hiding (Class)
import qualified Solcore.Frontend.TypeInference.AbsFun as A
import qualified Solcore.Frontend.TypeInference.ISyntax as I 
import Solcore.Frontend.TypeInference.Desugar (desugar)

-- compiling source language into type inference 

internalCompiler :: CompUnit -> [I.Prog]
internalCompiler = map (desugar . sccAnalysis) . translate

-- strong connect component analysis for building mutual blocks 

sccAnalysis :: A.Prog -> A.Prog 
sccAnalysis (A.Prog ds) 
  = A.Prog (newDecls ++ others)
    where 
      isFun (A.ValBind _ _ _) = True 
      isFun _ = False 
      (funs, others) = partition isFun ds
      (cgraph, posMap, declMap) = mkCallGraph funs
      scss = scc cgraph
      newDecls = rebuildDecls posMap declMap scss 

rebuildDecls :: Map Int A.LIdent -> Map A.LIdent A.Decl -> [[Node]] -> [A.Decl]
rebuildDecls posMap declMap 
  = map (rebuildDecl posMap declMap) 

rebuildDecl :: Map Int A.LIdent -> Map A.LIdent A.Decl -> [Node] -> A.Decl 
rebuildDecl _ _ [] 
  = error "Impossible! Empty node list!"
rebuildDecl pmap dmap [n] 
  = rebuild pmap dmap n 
rebuildDecl pmap dmap ns 
  = A.Mutual (map (rebuild pmap dmap) ns)

rebuild :: Map Int A.LIdent -> Map A.LIdent A.Decl -> Node -> A.Decl 
rebuild pmap dmap n 
  = case Map.lookup n pmap of
      Just k -> 
        case Map.lookup k dmap of
          Just d -> d 
          Nothing -> error "Impossible! Undefined decl" 
      Nothing -> error "Impossible! Undefined decl"

mkCallGraph :: [A.Decl] -> (Gr A.LIdent (), Map Int A.LIdent, Map A.LIdent A.Decl)
mkCallGraph ds 
  = (mkGraph nodes' edges', keyMap, declMap)
    where
      declMap = foldr (\ d ac -> Map.insert (nameOf d) d ac) Map.empty ds
      keyMap = Map.fromList nodes'
      valMap = Map.fromList (map (\ (x,y) -> (y,x)) nodes')
      table = mkCallTable ds 
      nodes' = zip [0..] (map nameOf ds)
      edges' = mkEdges table valMap

mkEdges :: Map A.LIdent [A.LIdent] -> Map A.LIdent Int -> [LEdge ()] 
mkEdges table pos 
  = Map.foldrWithKey go [] table 
    where 
      findPos k = Map.findWithDefault (-1) k pos 
      go k v ac = map (\ x -> (findPos k, findPos x, ())) v ++ ac

mkCallTable :: [A.Decl] -> Map A.LIdent [A.LIdent]
mkCallTable ds 
  = foldr go emptyTable ds  
    where
      emptyTable = foldr (flip Map.insert [] . nameOf) Map.empty ds 
      funs = Map.keys emptyTable 
      go (A.ValBind n _ e) ac 
        = Map.insert n (fv e `intersect` funs) ac

nameOf :: A.Decl -> A.LIdent 
nameOf (A.ValBind n _ _) = n
nameOf d = error $ "invalid declaration:" ++ show d

class FreeVars a where 
  fv :: a -> [A.LIdent]

instance FreeVars a => FreeVars [a] where 
  fv = foldr (union . fv) []

instance FreeVars A.Expr where 
  fv (A.EBlock ss) = fv ss 
  fv (A.ELam _ e) = fv e 
  fv (A.ECase e eqns) = fv e `union` fv eqns  
  fv (A.EApp e1 e2) = fv e1 `union` fv e2 
  fv (A.EMet e1 e2) = fv e1 `union` fv e2
  fv (A.EVar v) = [v]
  fv _ = []

instance FreeVars A.CaseAlt where 
  fv (A.CaseAlt _ _ e) = fv e

instance FreeVars A.Stmt where 
  fv (A.SExpr e) = fv e 
  fv (A.SAssign _ e2) = fv e2
  fv (A.SInit _ e2) = fv e2 
  fv _ = []

-- translating into type checker internal syntax

class Translate a where 
  type Res a 
  translate :: a -> Res a 

instance Translate a => Translate [a] where 
  type Res [a] = [Res a]
  translate = map translate 

instance Translate CompUnit where 
  type Res CompUnit = [A.Prog]

  translate (CompUnit _ ds) = map translate ds 

instance Translate Contract where 
  type Res Contract = A.Prog

  translate (Contract _ _ decls) 
    = A.Prog (concat $ translate decls')
      where 
        decls' = filter canTranslate decls 

canTranslate :: Decl -> Bool
canTranslate (ConstrDecl _) = False 
canTranslate (SymDecl _) = False 
canTranslate _ = True 

instance Translate Decl where 
  type Res Decl = [A.Decl]

  translate (DataDecl dt) = [translate dt]
  translate (ClassDecl cls) = [translate cls]
  translate (InstDecl ins) = [translate ins]
  translate (FunDecl fun) = translate fun
  translate (FieldDecl fd) = translate fd 
  translate _ = error "Invalid declaration."

instance Translate Field where 
  type Res Field = [A.Decl]  

  translate (Field n t Nothing) 
    = [A.ValDecl (A.LIdent (unName n)) 
                 (A.T0Qual (translate t))]
  translate (Field n t (Just e)) = [] -- XXX continue here

instance Translate DataTy where 
  type Res DataTy = A.Decl 
  translate (DataTy n vs cons) 
    = A.TypeDecl t cons' 
      where 
        t = A.CTCon (A.UIdent (unName n)) 
                    (map (translate . TyVar) vs)
        cons' = A.ConAlts (translate cons)

instance Translate Constr where 
  type Res Constr = A.ConAlt  

  translate (Constr n []) 
    = A.ConAlt0 (A.UIdent (unName n)) 
  translate (Constr n ts) 
    = A.ConAltN (A.UIdent (unName n)) (translate ts)

instance Translate Ty where 
  type Res Ty = A.CType
  translate (TyVar v) = A.CTVar (A.LIdent (unName (unVar v))) 
  translate (t :-> t') = A.CTArr (translate t) (translate t') 
  translate (TyCon n []) = A.CTCon0 (A.UIdent (unName n))
  translate (TyCon n ts) = A.CTCon (A.UIdent (unName n)) (translate ts) 

instance Translate Class where 
  type Res Class = A.Decl 
  translate (Class ps n vs v sigs) 
    = A.ClsDecl p sigs'
      where 
        p = mkPred n (TyVar <$> vs) (TyVar v) 
        sigs' = A.SomeMethods (translate sigs)
        

mkPred :: Name -> [Ty] -> Ty -> A.CPred 
mkPred m [] v1 = A.PSingle (translate v1) 
                           (A.UIdent (unName m))
mkPred m ts v1 = A.PMulti (translate v1) 
                          (A.UIdent (unName m))
                          (translate ts)

instance Translate Signature where 
  type Res Signature = A.Decl 
  translate (Signature n ps t) 
    = A.ValDecl (A.LIdent (unName n)) 
                (A.T0Qual $ foldr A.CTArr t' ts')
      where
        ts' = map (translate . snd) ps
        t' = maybe uTy translate t
        uTy = A.CTCon0 (A.UIdent "Unit")


instance Translate Instance where 
  type Res Instance = A.Decl 
  translate (Instance ps n ts t funs) 
    = A.InstDecl (mkQPred ps n ts t) (A.SomeMethods funs')
      where 
        funs' = concat $ translate funs

mkQPred :: [Pred] -> Name -> [Ty] -> Ty -> A.QPred 
mkQPred [] n ts t = A.I0Qual (mkPred n ts t)
mkQPred [p] n ts t 
  = A.I1Qual (pred2QPred p) (mkPred n ts t) 
mkQPred ps n ts t = A.INQual (pred2QPred <$> ps) (mkPred n ts t) 

pred2QPred :: Pred -> A.CPred 
pred2QPred p 
  = mkPred (predName p) (predParams p) (predMain p)

instance Translate FunDef where 
  type Res FunDef = [A.Decl]
  translate (FunDef n rt ps ss) 
    = [A.ValDecl n' qt, A.ValBind n' (f <$> ps) (A.EBlock ss')]
      where 
        ss' = concat $ translate ss
        n' = A.LIdent (unName n)
        qt = A.T0Qual $ foldr A.CTArr t' ts' 
        ts' = map (translate . snd) ps
        t' = maybe (A.CTCon0 (A.UIdent "Unit")) translate rt
        f (n, t) = A.TArg (A.LIdent (unName n)) (translate t)

instance Translate Stmt where 
  type Res Stmt = [A.Stmt]
  translate (lhs := rhs) 
    = [A.SAssign (translate lhs) (translate rhs)]
  translate (Let n (Just t) Nothing) 
    = [A.SAlloc (A.LIdent (unName n)) (translate t)]
  translate (Let n Nothing (Just e)) 
    = [A.SInit (A.LIdent (unName n)) (translate e)]
  translate (Let n (Just t) (Just e)) 
    = [ A.SAlloc (A.LIdent (unName n)) (translate t)
      , A.SAssign (A.EVar (A.LIdent (unName n))) (translate e)
      ]
  translate (StmtExp e) 
    = [A.SExpr (translate e)]
  translate (Match [e] eqns) 
    = [A.SExpr $ A.ECase (translate e) (translate eqns)]


instance Translate Equation where 
  type Res Equation = A.CaseAlt 
  translate ([PCon n ps], ss) 
    = A.CaseAlt (A.UIdent (unName n)) 
                (map pat2Arg ps) 
                (A.EBlock $ concat $ translate ss)
  translate p = error ("unsupported pattern:" ++ show p)

pat2Arg :: Pat -> A.Arg
pat2Arg (PVar v) = A.UArg (A.LIdent (unName v))

instance Translate Exp where 
  type Res Exp = A.Expr 
  translate (Lit (IntLit i)) = A.EInt i 
  translate (Lit (StrLit _)) = A.EInt (-1)-- error "String literals aren't supported yet!"
  translate (Var n) = A.EVar (A.LIdent (unName n))
  translate (Con n es) = foldl (\ ac e -> A.EApp ac (translate e)) 
                               (A.ECon (A.UIdent (unName n))) 
                               es
  translate (FieldAccess e n) = A.EMet (translate e) 
                                       (A.EVar (A.LIdent (unName n)))
  translate (Call me n es) = f (foldl (\ac e -> A.EApp ac (translate e))
                                      (A.EVar (A.LIdent (unName n)))
                                      es)
                  where 
                    f = maybe id (A.EMet . translate) me
  translate (Lam args _ body) 
    = A.ELam args' body'
      where
        f n = A.LIdent (unName n)
        args' = (A.UArg . f . fst) <$> args
        body' = A.EBlock (concat $ translate body)
