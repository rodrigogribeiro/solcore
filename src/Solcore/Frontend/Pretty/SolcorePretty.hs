module Solcore.Frontend.Pretty.SolcorePretty where 

import Data.List

import Prelude hiding ((<>))

import Solcore.Frontend.Syntax.Contract
import Solcore.Frontend.Syntax.Name 
import Solcore.Frontend.Syntax.Stmt 
import Solcore.Frontend.Syntax.Ty hiding (Class)

import Text.PrettyPrint.HughesPJ

-- top level pretty printer function 

pretty :: Pretty a => a -> String 
pretty = render . ppr

class Pretty a where 
  ppr :: a -> Doc  

instance Pretty a => Pretty (Maybe a) where 
  ppr Nothing = empty 
  ppr (Just x) = ppr x

instance Pretty CompUnit where 
  ppr (CompUnit imps cs)
    = vcat (map ppr imps ++ map ppr cs)

instance Pretty Import where 
  ppr (Import qn) 
    = text "import" <+> ppr qn <+> semi

instance Pretty Contract where 
  ppr (Contract n ts ds)
    = text "contract" <+> 
      ppr n <+> 
      pprTyParams (map TyVar ts) <+> 
      lbrace $$ 
      nest 3 (vcat (map ppr ds)) $$ 
      rbrace 

instance Pretty Decl where 
  ppr (DataDecl dt)
    = ppr dt 
  ppr (SymDecl ts)
    = ppr ts 
  ppr (ClassDecl cd)
    = ppr cd 
  ppr (InstDecl id)
    = ppr id 
  ppr (FieldDecl fd)
    = ppr fd 
  ppr (FunDecl fd)
    = ppr fd
  ppr (ConstrDecl c)
    = ppr c 

instance Pretty Constructor where 
  ppr (Constructor ps bd)
    =  text "constructor" <+> 
       pprParams ps <+> 
       lbrace $$ 
       nest 3 (vcat (map ppr bd)) $$ 
       rbrace

instance Pretty DataTy where 
  ppr (DataTy n ps cs)
    = text "data" <+> 
      ppr n <+>
      pprTyParams (map TyVar ps) <+> 
      equals <+>  
      hsep (punctuate bar (map ppr cs))
    where 
      bar = text "|"

instance Pretty Constr where
  ppr (Constr n []) = ppr n <> text " "
  ppr (Constr n ts)
    = ppr n <> brackets (pprConstrArgs ts)

pprConstrArgs :: [Ty] -> Doc  
pprConstrArgs [] = empty 
pprConstrArgs ts = commaSep $ map ppr ts 

instance Pretty TySym where 
  ppr (TySym n vs ty)
    = hsep [
            text "type"
           , ppr n 
           , pprTyParams (map TyVar vs) 
           , equals
           , ppr ty 
           ]

instance Pretty Class where 
  ppr (Class ps n vs v sigs)
    = text "class " <+> 
      pprContext ps <+> 
      ppr v <+> 
      colon <+> 
      ppr n <+> 
      lbrace $$ 
      pprSignatures sigs $$  
      rbrace 

pprSignatures :: [Signature] -> Doc 
pprSignatures 
  = vcat . map ppr

instance Pretty Signature where 
  ppr (Signature n ps ty)
    = text "function" <+> 
      ppr n           <+> 
      pprParams ps    <+> 
      pprRetTy ty   

instance Pretty Instance where 
  ppr (Instance ctx n tys ty funs)
    = text "instance" <+> 
      pprContext ctx  <+> 
      ppr ty          <+>
      colon           <+> 
      ppr n           <+> 
      pprTyParams tys <+> 
      lbrace          $$ 
      nest 3 (pprFunBlock funs) $$ 
      rbrace 

pprContext :: [Pred] -> Doc 
pprContext [] = empty 
pprContext ps 
  = parens $ (commaSep $ map ppr ps) <+> text "=>"

pprFunBlock :: [FunDef] -> Doc 
pprFunBlock 
  = vcat . map ppr

instance Pretty Field where 
  ppr (Field n ty _)
    = ppr n <+> colon <+> (ppr ty)

instance Pretty FunDef where 
  ppr (FunDef n ty ps bd)
    = text "function" <+> 
      ppr n <+> 
      pprParams ps <+> 
      pprRetTy ty <+>
      lbrace $$ 
      nest 3 (vcat (map ppr bd)) $$ 
      rbrace

pprRetTy :: Maybe Ty -> Doc  
pprRetTy (Just t) = text "->" <+> ppr t
pprRetTy _        = empty 

pprParams :: [Param] -> Doc  
pprParams = parens . commaSep . map pprParam

pprParam :: Param -> Doc 
pprParam (n, ty) 
  = ppr n <+> colon <+> ppr ty

instance Pretty Stmt where 
  ppr (n := e) 
    = ppr n <+> equals <+> ppr e <+> semi 
  ppr (Let n ty m)
    = text "let" <+> ppr n <+> ppr ty <+> pprInitOpt m 
  ppr (StmtExp e)
    = ppr e <> semi
  ppr (Return e)
    = text "return" <+> ppr e
  ppr (Match e eqns)
    = text "match" <+> 
      (parens $ commaSep $ map ppr e) <+> 
      lbrace $$ 
      vcat (map ppr eqns) $$ 
      rbrace 

instance Pretty Equation where 
  ppr (p,ss) 
    = text "|" <+> commaSep (map ppr p) <+> text "=>" $$ 
      nest 3 (vcat (map ppr ss))

pprInitOpt :: Maybe Exp -> Doc
pprInitOpt Nothing = semi
pprInitOpt (Just e) = equals <+> ppr e <+> semi 

instance Pretty Exp where 
  ppr (Var v) = ppr v 
  ppr (Con n es) 
    = ppr n <> (brackets $ commaSep $ map ppr es)
  ppr (Lit l) = ppr l 
  ppr (Call e n es) 
    = pprE e <> ppr n <> (parens $ commaSep $ map ppr es)
  ppr (Lam ps t bd)
    = text "lam" <+> pprLamParams ps <+> text "->" <+> 
      ppr t <+> lbrace $$ nest 3 (vcat (map ppr bd)) $$ rbrace 

pprLamParams :: [(Name, Maybe Ty)] -> Doc 
pprLamParams 
  = parens . commaSep . map pprOptTyParam 
    where 
      pprOptTyParam (n, Just mt) = ppr n <+> colon <+> ppr mt 
      pprOptTyParam (n, Nothing) = ppr n

pprE :: Maybe Exp -> Doc  
pprE Nothing = ""
pprE (Just e) = ppr e <> text "."

instance Pretty Pat where 
  ppr (PVar n) 
    = ppr n
  ppr (PCon n []) = ppr n
  ppr (PCon n ps@(_ : _)) 
    = ppr n <> (brackets $ commaSep $ map ppr ps )
  ppr PWildcard 
    = text "_"
  ppr (PLit l)
    = ppr l

instance Pretty Literal where 
  ppr (IntLit l) = integer (toInteger l)
  ppr (StrLit l) = quotes (text l)

instance Pretty Tyvar where 
  ppr (TVar n) = ppr n 

instance Pretty Pred where 
  ppr (InCls n t ts) =
    ppr t <+> colon <+> ppr n <+> pprTyParams ts 
  ppr (t1 :~: t2) = ppr t1 <+> text "~" <+> ppr t2

instance Pretty Ty where 
  ppr (TyVar v) = ppr v 
  ppr (TyCon n ts)
    | n == arr = pprArrTy (unsnoc ts)
    | otherwise 
      = ppr n <> (pprTyParams ts)

pprArrTy :: Maybe ([Ty], Ty) -> Doc 
pprArrTy Nothing = empty 
pprArrTy (Just (ts, t))
    = parens (commaSep (map ppr ts)) <+> text "->" <+> ppr t 

pprTyParams :: [Ty] -> Doc 
pprTyParams [] = empty 
pprTyParams ts 
  = brackets (commaSep (map ppr ts))

instance Pretty Name where 
  ppr = text . unName

instance Pretty QualName where 
  ppr = dotSep . map ppr . unQName


dotSep :: [Doc] -> Doc
dotSep = hcat . punctuate dot 
         where 
          dot = text "."

commaSep :: [Doc] -> Doc
commaSep = hsep . punctuate comma 


