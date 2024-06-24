{-# LANGUAGE UndecidableInstances #-}
module Solcore.Frontend.TypeInference.ISyntax where

import Data.List(union, intersect, nub, (\\), intercalate)

import Solcore.Frontend.Syntax.Ty
import Solcore.Frontend.Syntax.Name 
import Solcore.Frontend.TypeInference.Constraints (HasType(..))

data Expr
    = ELam [Arg] Expr        -- function \args -> expr
    | ELet Name Expr Expr    -- local definition: let name = expr1 in expr2
    | EApp Expr Expr         -- function call: f(arg)
    | EVar Name              -- variable
    | ECon Name              -- value constructor
    | EInt Integer           -- integer literal
    | EBlock [Stmt String]   -- desugared statements annotated with their source form
    | ETyped Expr Ty
    | ECase Expr [CaseAlt]
  deriving Eq

data Arg = UArg Name | TArg Name Ty deriving Eq

-- case alternative: constructor name, bound variables, expression

data CaseAlt = CaseAlt Name [Arg] Expr deriving Eq

data Stmt ann             -- ann - annotation (e.g. stmt before desugar)
    = SExpr ann Expr
    | SAssign ann Expr Expr
    | SAlloc ann Name Ty
    | SInit ann Name Expr
  deriving (Eq)

data Decl
    = TypeDecl Ty [ConAlt]
    | ValDecl Name (Qual Ty)
    | ValBind Name [Arg] Expr
    | Mutual [Decl]
    | InstDecl (Qual Pred) [Decl]
    | ClsDecl Pred [Decl]
    | Pragma String
  deriving (Eq, Show)

data Bind = Bind { bindName :: Name 
                 , bindArgs :: [Arg]
                 , bindBody :: Expr }
  deriving (Eq, Show)

data ConAlt = ConAlt Name [Ty]
  deriving (Eq, Show)


newtype Prog = Prog [Decl]

instance Show Prog where 
  show (Prog ds) = unlines $ map show ds 

instance Show Expr where
  showsPrec d (EInt n) = showsPrec 10 n
  showsPrec d (EVar n) = showString (unName n)
  showsPrec d (ECon n) = showString (unName n)
  showsPrec d (EApp e1 e2) = showParen (d > ap_prec) $
             showsPrec ap_prec e1   .
             showString " "           .
             showsPrec (ap_prec+1) e2
         where ap_prec = 10

  showsPrec d (ELam args e) = showParen (d > lam_prec) $
             showString "\\" . showArgs args . showString " -> " .
             showsPrec lam_prec e
         where
           lam_prec = 1
           showArgs = showString . unwords  . map showArg

  showsPrec d (ELet x e1 e2) = showParen (d > let_prec) $
             showString "let " . showString (unName x) . showString "= " . showsPrec 0 e1 .
             showsPrec let_prec e2
         where let_prec = 2

  showsPrec d (EBlock stmts) = showString "{\n  " .
                               showString ( intercalate ";\n  " (map show stmts)) .
                               showString "\n}"
  showsPrec d (ETyped e t) = showParen (d > typ_prec) $
             showsPrec 0 e .
             showString " : " . showsPrec 10 t
         where typ_prec = 2
  showsPrec d (ECase e alts) = showString "case " . showsPrec 0 e . showString " of {" .
                               showString ( intercalate ";\n  " (map show alts)) . ('}' :)

showExpr :: Expr -> String
showExpr e = show e

instance Show (Stmt ann) where
  show :: Stmt ann -> String
  show (SExpr _ e) = showExpr e
  show (SAlloc _ x t) = concat ["let ",  unName x, " : ", show t]
  show (SInit _ n e) = concat [unName n, " = ", show e] 

instance Show Arg where 
  show :: Arg -> String
  show = showArg

showArg :: Arg -> String
showArg (UArg s) = unName s
showArg (TArg s t) = concat ["(",unName s,":",show t,")"]

instance Show CaseAlt where
  show (CaseAlt c args e) 
    = concat [unName c, " ", unwords (map show args), " -> ", show e]

showDecl :: Decl -> String 
showDecl (ValDecl n qt) = unwords [unName n, ":", show qt]
showDecl (ValBind n [] e) = unwords [unName n, "=", show e]
showDecl (ValBind n as e) = unwords [unName n, sas, "=", show e] 
  where
    sas = unwords (map showArg as)
showDecl (ClsDecl pred mdecls) = unwords ["class", show pred, "{",  showDecls mdecls, "}"] 
  where
    showDecls ds = intercalate "; " (map showDecl ds)
showDecl (InstDecl pred mdecls) = unwords ["instance", show pred] where
showDecl d = show d

argName :: Arg -> Name
argName (UArg s) = s
argName (TArg s t) = s

class ToStr a where
  str :: a -> String

instance {-# OVERLAPPABLE  #-} Show a => ToStr a where str = show
instance {-# OVERLAPPING   #-} ToStr String where str = id
instance {-# OVERLAPPING   #-} ToStr Expr where str = showExpr
instance {-# OVERLAPPING   #-} ToStr Decl where str = showDecl

class HasFreeVars a where
    freeVars :: a -> [Name]

instance HasFreeVars Expr where
    freeVars (EVar n) = [n]
    freeVars (EApp e1 e2) = freeVars e1 `union` freeVars e2
    freeVars (ELam args e) = freeVars e \\ map argName args
    freeVars _ = [] -- FIXME
    -- freeVars e = error("freeVars unimplemented for: " ++show e)


instance HasType Arg where
    apply s (UArg n) = UArg n
    apply s (TArg n t) = TArg n (apply s t)
    ftv (UArg n) = []
    ftv (TArg n t) = ftv t

instance HasType Expr where
    apply s (EInt i) = EInt i
    apply s (EVar n) = EVar n
    apply s (ECon n) = ECon n
    apply s (EApp e1 e2) = EApp (apply s e1) (apply s e2)
    apply s (ELam args e) = ELam (apply s args) (apply s e)
    apply s (ELet n e1 e2) = ELet n (apply s e1) (apply s e2)
    apply s (ETyped e t) = ETyped (apply s e) (apply s t)
    apply s (EBlock stmts) = EBlock (apply s stmts)
    ftv (EInt _) = []
    ftv (EVar n) = []
    ftv (ECon n) = []
    ftv (EApp e1 e2) = ftv e1 ++ ftv e2
    ftv (ELam args e) = ftv args ++ ftv e
    ftv (ELet n e1 e2) = ftv e1 ++ ftv e2
    ftv (ETyped e t) = ftv e ++ ftv t
    ftv (EBlock stmts) = ftv stmts

instance HasType (Stmt ann) where
    apply s (SExpr ann e) = SExpr ann (apply s e)
    apply s (SAlloc ann n t) = SAlloc ann n (apply s t)
    ftv (SExpr _ e) = ftv e
    ftv (SAlloc _ n t) = ftv t
