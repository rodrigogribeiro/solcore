module Solcore.Frontend.Pretty.SolcorePretty where 

import Solcore.Frontend.Syntax.Contract
import Solcore.Frontend.Syntax.Name 
import Solcore.Frontend.Syntax.Stmt 
import Solcore.Frontend.Syntax.Ty
import Text.PrettyPrint.HughesPJ hiding ((<>)) 

-- top level pretty printer function 

pretty :: Pretty a => a -> String 
pretty = render . ppr'

class Pretty a where 
  ppr :: Int -> a -> Doc 

  ppr' :: a -> Doc 

  ppr' = ppr 0

instance Pretty CompUnit where 
  ppr _ (CompUnit imps cs)
    = hcat (punctuate nl (map ppr' imps ++ map ppr' cs))

instance Pretty Import where 
  ppr _ (Import qn) 
    = text "import" <+> ppr' qn <+> semi

instance Pretty Contract where 
  ppr d (Contract n ts ds)
    = hsep [
             nest d $ text "contract"
           , ppr' n 
           , pprTyParams (map TyVar ts) 
           , text "{\n"
           , hcat ds'
           , text "\n}"
           ]
      where 
        ds' = punctuate nl $ map (ppr (d + 3)) ds

instance Pretty Decl where 
  ppr d (DataDecl dt)
    = ppr d dt 
  ppr d (SymDecl ts)
    = ppr d ts 
  ppr d (ClassDecl cd)
    = ppr d cd 
  ppr d (InstDecl id)
    = ppr d id 
  ppr d (FieldDecl fd)
    = ppr d fd 
  ppr d (FunDecl fd)
    = ppr d fd

instance Pretty DataTy where 
  ppr d (DataTy n ps cs)
    = hsep $ [
               nest d $ text "data"
             , pprTyParams (map TyVar ps) 
             , equals 
             ] ++ cs' 
    where 
      cs' = punctuate bar $ map ppr' cs 
      bar = text "|"

instance Pretty Constr where 
  ppr d (Constr n ts)
    = hsep [
             nest d $ ppr' n 
           , text "("
           , commaSep $ map ppr' ts 
           , text ")"
           ]

instance Pretty TySym where 
  ppr d (TySym n vs ty)
    = hsep [
             nest d $ text "type"
           , ppr' n 
           , pprTyParams (map TyVar vs) 
           , equals
           , ppr' ty 
           ]

instance Pretty Class where 
  ppr d (Class ps n vs v sigs)
    = hsep [
             nest d $ text "class"
           , pprContext ps 
           , ppr' v 
           , colon 
           , ppr' n 
           , text "{\n"
           , pprSignatures (d + 3) sigs 
           , text "\n}"
           ]

pprSignatures :: Int -> [Signature] -> Doc 
pprSignatures d 
  = hcat . punctuate nl . map (ppr d)

instance Pretty Signature where 
  ppr d (Signature n ps ty)
    = hsep [
             function d  
           , ppr' n 
           , pprParams ps 
           , text "->" 
           , ppr' ty 
           ]

instance Pretty Instance where 
  ppr d (Instance ctx n tys ty funs)
    = hsep [  
             pinst 
           , pprContext ctx
           , text "=>"
           , ppr' ty  
           , colon 
           , ppr' n 
           , pprTyParams tys 
           , text "{\n"
           , pprFunBlock (d + 3) funs 
           , text "\n}"
           ]
      where 
        pinst = nest d (text "instance")

pprContext :: [Pred] -> Doc 
pprContext ps 
  = parens $ commaSep $ map ppr' ps    

pprFunBlock :: Int -> [FunDef] -> Doc 
pprFunBlock d 
  = hcat . punctuate nl . map (ppr d)

instance Pretty Field where 
  ppr d (Field n ty _)
    = (nest d $ ppr' n) <+> colon <+> ppr' ty 

instance Pretty FunDef where 
  ppr d (FunDef n ty ps bd)
    = hsep [ function d  
           , ppr' n 
           , pprParams ps
           , ty'
           , pprBody d bd 
           ]
    where
      ty' = text "->" <+> ppr' ty

pprParams :: [Param] -> Doc 
pprParams = parens . commaSep . map pprParam

pprParam :: Param -> Doc 
pprParam (n, ty) 
  = ppr' n <+> colon <+> ppr' ty

instance Pretty Stmt where 
  ppr d (n := e) 
    = nest d $ (ppr' n <+> equals <+> ppr' e) <> semi 
  ppr d (If e bd) 
    = nest d $ text "if" <+> parens (ppr' e) <+> pprBody (d + 3) bd 
  ppr d (While e bd)
    = nest d $ text "while" <+> parens (ppr' e) <+> pprBody d bd 
  ppr d (StmtExp e)
    = nest d $ ppr d e <> semi 

pprBody :: Int -> Body -> Doc 
pprBody d bd = nest d (text "{\n") <+> 
               bd' <+>
               nest d (text "}\n") 
      where 
        bd' = hcat $ punctuate (semi <> nl) (map (ppr (d + 3)) bd)

instance Pretty Exp where 
  ppr _ (Var v) = ppr' v 
  ppr _ (Con n es) 
    = ppr' n <> (parens $ commaSep $ map ppr' es)
  ppr _ (Lit l) = ppr' l 
  ppr _ (Call n es) 
    = ppr' n <> (parens $ commaSep $ map ppr' es)
  ppr d (Switch e eqns)
    = switch <+> ppr' e <+> pprSwitchBlock (d + 3) eqns 
      where 
      switch = nest d (text "switch")

pprSwitchBlock :: Int -> [(Pat,[Stmt])] -> Doc 
pprSwitchBlock d es = (nest d $ text "{\n") <> 
                      (hcat $ map (pprCase (d + 3)) es) <> 
                      (nest d $ text "}\n")

pprCase :: Int -> (Pat, [Stmt]) -> Doc 
pprCase d (p,ss) 
  = (nest d $ text "case") <+> ppr' p <+> colon <+> nl <+> ss' 
    where 
      ss' = hcat $ punctuate (semi <> nl) (map (ppr (d + 3)) ss)

instance Pretty Pat where 
  ppr _ (PVar n) 
    = ppr' n
  ppr _ (PCon n ps) 
    = ppr' n <> (parens $ commaSep $ map ppr' ps )
  ppr _ PWildcard 
    = text "_"
  ppr _ (PLit l)
    = ppr' l

instance Pretty Literal where 
  ppr _ (IntLit l) = integer l

instance Pretty Tyvar where 
  ppr _ (TVar n) = ppr' n 

instance Pretty Pred where 
  ppr _ (Pred n t ts) =
    hsep [
           ppr' t
         , colon 
         , ppr' n 
         , pprTyParams ts
         ]

instance Pretty Ty where 
  ppr _ (TyVar v) = ppr' v 
  ppr _ (TyCon n ts)
    = ppr' n <> (pprTyParams ts)

pprTyParams :: [Ty] -> Doc 
pprTyParams [] = empty 
pprTyParams ts 
  = brackets (commaSep (map ppr' ts))

instance Pretty Name where 
  ppr _ = text . unName

instance Pretty QualName where 
  ppr _ = dotSep . map ppr' . unQName


dotSep :: [Doc] -> Doc 
dotSep = hcat . punctuate dot 
         where 
          dot = text "."

commaSep :: [Doc] -> Doc 
commaSep = hcat . punctuate comma 

nl :: Doc
nl = text "\n"

function :: Int -> Doc 
function d 
  = nest d $ text "function"
