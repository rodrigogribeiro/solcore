module Solcore.Frontend.Pretty.SolcorePretty where 

import Data.List

import Solcore.Frontend.Syntax.Contract
import Solcore.Frontend.Syntax.Name 
import Solcore.Frontend.Syntax.Stmt 
import Solcore.Frontend.Syntax.Ty


-- top level pretty printer function 

pretty :: Pretty a => a -> String 
pretty = ppr'

class Pretty a where 
  ppr :: Int -> a -> String  

  ppr' :: a -> String  

  ppr' = ppr 0

instance Pretty CompUnit where 
  ppr _ (CompUnit imps cs)
    = unlines (map ppr' imps ++ map ppr' cs)

instance Pretty Import where 
  ppr _ (Import qn) 
    = "import" <+> ppr' qn <+> semi

instance Pretty Contract where 
  ppr d (Contract n ts ds)
    = concat [
             nest d "contract "
           , ppr' n 
           , " "
           , pprTyParams (map TyVar ts) 
           , " {\n"
           , ds'
           , "}"
           ]
      where 
        ds' = unlines $ map (ppr (d + 3)) ds

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
  ppr d (ConstrDecl c)
    = ppr d c 

instance Pretty Constructor where 
  ppr d (Constructor ps bd)
    = unwords [
               nest d $ "constructor"
              , pprParams ps 
              , pprBody d bd 
              ]

instance Pretty DataTy where 
  ppr d (DataTy n ps cs)
    = unwords $ [
                nest d "data"
              , ppr' n
              , pprTyParams (map TyVar ps) 
              , equals 
              ] ++ cs' 
    where 
      cs' = intersperse bar $ map ppr' cs 
      bar = "|"

instance Pretty Constr where 
  ppr d (Constr n ts)
    = ppr' n <+> pprConstrArgs ts 

pprConstrArgs :: [Ty] -> String 
pprConstrArgs [] = ""
pprConstrArgs ts = commaSep $ map ppr' ts 

instance Pretty TySym where 
  ppr d (TySym n vs ty)
    = unwords [
             nest d "type"
           , ppr' n 
           , pprTyParams (map TyVar vs) 
           , equals
           , ppr' ty 
           ]

instance Pretty Class where 
  ppr d (Class ps n vs v sigs)
    = concat [
             nest d "class "
           , pprContext ps 
           , ppr' v 
           , colon 
           , ppr' n 
           , " {\n"
           , pprSignatures (d + 3) sigs 
           , nest d "}"
           ]

pprSignatures :: Int -> [Signature] -> String 
pprSignatures d 
  = unlines . map (ppr d)

instance Pretty Signature where 
  ppr d (Signature n ps ty)
    = concat [
             function d  
           , " "
           , ppr' n 
           , " "
           , pprParams ps 
           , " -> "
           , ppr' ty 
           ]

instance Pretty Instance where 
  ppr d (Instance ctx n tys ty funs)
    = concat [  
             pinst 
           , pprContext ctx
           , ppr' ty  
           , colon 
           , ppr' n
           , " "
           , pprTyParams tys 
           , "{\n"
           , pprFunBlock (d + 3) funs 
           , nest d "}"
           ]
      where 
        pinst = nest d "instance "

pprContext :: [Pred] -> String
pprContext [] = ""
pprContext ps 
  = parens $ (commaSep $ map ppr' ps) ++ " => "

pprFunBlock :: Int -> [FunDef] -> String 
pprFunBlock d 
  = unlines . map (ppr d)

instance Pretty Field where 
  ppr d (Field n ty _)
    = nest d $ ppr' ty <+> (ppr' n)

instance Pretty FunDef where 
  ppr d (FunDef n ty ps bd)
    = concat [ function d 
           , " "
           , ppr' n 
           , " "
           , pprParams ps
           , " "
           , pprRetTy ty
           , pprBody d bd 
           ]

pprRetTy :: Maybe Ty -> String 
pprRetTy (Just t) = "->" <+> (ppr' t ++ " ")
pprRetTy _        = "" 

pprParams :: [Param] -> String 
pprParams = parens . commaSep . map pprParam

pprParam :: Param -> String 
pprParam (n, ty) 
  = ppr' n <+> colon <+> ppr' ty

instance Pretty Stmt where 
  ppr d (n := e) 
    = nest d $ (ppr' n <+> equals <+> ppr' e)
  ppr d (Let n ty m)
    = nest d $ "let" <+> ppr' n <+> colon <+> ppr' ty <+> pprInitOpt m 
  -- ppr d (If e bd) 
    -- = nest d $ "if" <+> parens (ppr' e) <+> pprBody (d + 3) bd 
  -- ppr d (While e bd)
    -- = nest d $ "while" <+> parens (ppr' e) <+> pprBody d bd 
  ppr d (StmtExp e)
    = nest d $ ppr d e <> semi 

pprInitOpt :: Maybe Exp -> String 
pprInitOpt Nothing = ""
pprInitOpt (Just e) = "=" <+> ppr' e 

pprBody :: Int -> Body -> String 
pprBody d bd = "{\n" ++ 
               bd'   ++ 
               '\n' : nest d "}" 
      where 
        bd' = intercalate (semi <> nl) (map (ppr (d + 3)) bd)

instance Pretty Exp where 
  ppr _ (Var v) = ppr' v 
  ppr _ (Con n es) 
    = ppr' n <> (brackets $ commaSep $ map ppr' es)
  ppr _ (Lit l) = ppr' l 
  ppr _ (Call n es) 
    = ppr' n <> (parens $ commaSep $ map ppr' es)
  ppr d (Switch e eqns)
    = "switch" <+> ppr' e <+> pprSwitchBlock d eqns 

pprSwitchBlock :: Int -> [(Pat,[Stmt])] -> String 
pprSwitchBlock d es = "{\n" <> 
                      (intercalate "\n" $ map (pprCase (d + 3)) es) <> 
                      ('\n' : nest d "}")

pprCase :: Int -> (Pat, [Stmt]) -> String 
pprCase d (p,ss) 
  = (nest d "case") <+> ppr' p <+> colon <+> nl <+> ss' 
    where 
      ss' = intercalate (semi <> nl) (map (ppr (d + 3)) ss)

instance Pretty Pat where 
  ppr _ (PVar n) 
    = ppr' n
  ppr _ (PCon n ps) 
    = ppr' n <> (brackets $ commaSep $ map ppr' ps )
  ppr _ PWildcard 
    = "_"
  ppr _ (PLit l)
    = ppr' l

instance Pretty Literal where 
  ppr _ (IntLit l) = show l

instance Pretty Tyvar where 
  ppr _ (TVar n) = ppr' n 

instance Pretty Pred where 
  ppr _ (Pred n t ts) =
      unwords [
           ppr' t
         , colon 
         , ppr' n 
         , pprTyParams ts
         ]

instance Pretty Ty where 
  ppr _ (TyVar v) = ppr' v 
  ppr _ (TyCon n ts)
    = ppr' n <> (pprTyParams ts)

pprTyParams :: [Ty] -> String 
pprTyParams [] = "" 
pprTyParams ts 
  = brackets (commaSep (map ppr' ts))

instance Pretty Name where 
  ppr _ = unName

instance Pretty QualName where 
  ppr _ = dotSep . map ppr' . unQName


dotSep :: [String] -> String 
dotSep = intercalate dot 
         where 
          dot = "."

commaSep :: [String] -> String 
commaSep = intercalate comma 

nl :: String
nl = "\n"

comma :: String 
comma = ", "

colon :: String 
colon = ":"

function :: Int -> String  
function d 
  = nest d "function"

-- basic functions 

nest :: Int -> String -> String 
nest n s = replicate n ' ' ++ s

parens :: String -> String 
parens s = "(" ++ s ++ ")"

brackets :: String -> String 
brackets s = "[" ++ s ++ "]"

semi :: String 
semi = ";"

equals :: String 
equals = "="

(<+>) :: String -> String -> String 
s1 <+> s2 = s1 ++ " " ++ s2
