module Solcore.Frontend.TypeInference.Specialise where
import Control.Monad.Error.Class(throwError)
import Data.List(intercalate)
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.TypeInference.TcMonad
import Solcore.Frontend.TypeInference.Checker
import Solcore.Frontend.TypeInference.ISyntax
import Solcore.Frontend.TypeInference.Constraints
import Solcore.Frontend.Syntax.Ty

specialiseEntry :: Name -> TCM ()
specialiseEntry name = do
  mdef <- lookupTLD name
  def@(name, scheme, args, body) <- maybeToTCM  ("No definition of " ++ unName name) mdef
  typ <- maybeToTCM ("Type of entry "++ (unName name) ++":"++ str scheme ++" is not monomorphic")
         (typeOfScheme scheme)
  warn ["! specialiseEntry ", unName name, " : ", str typ]
  body' <- specialiseBody args body typ
  info ["! new body: ", str body']
  let newDef = ValBind name args body'
  info ["! new def: ", str newDef]
  addSpecialisation name typ args body'
  return ()

specialiseBody :: [Arg] -> Expr -> Ty -> TCM Expr
specialiseBody args exp etyp = do
  env <- getEnv
  as <- addArgs args -- FIXME
  newBody <- specialiseExp exp (resultType etyp args)
  putEnv env
  return newBody

resultType :: Ty -> [Arg] -> Ty
resultType t [] = t
resultType (ta :-> tr) (a:as) = resultType tr as
resultType typ args = error (
  "resultType: wrong number of arguments; typ="++str typ++"++args="++str args)

specialiseExp :: Expr -> Ty -> TCM Expr
specialiseExp e@(EInt i) etyp = pure e
specialiseExp e@(EVar n) etyp = do
  mres <- lookupResolution n etyp
  case mres of
    Just (exp, subst) -> do
              fscheme <- askType n
              let (Forall _ (ps :=> typ)) = fscheme
              let tvs = ftv typ
              warn ["! specVar ", unName n, ":", str fscheme," @ ", str etyp,  " resolution: ", str (exp, subst)]
              phi <- mgu typ etyp `wrapError` ("specialise "++ unName n, typ, etyp)
              let subst' = subst <> phi
              let tvs' = apply subst' (map TyVar tvs)
              warn ["- specVar: tvs=", str tvs, " tvs'=", str tvs']
              let name' = specName n tvs'
              body' <- specialiseExp exp etyp
              warn ["< specExp ", str exp, " : ", str etyp, " ~>", str body']
              let args' = [] -- FIXME
              -- for noninlining version, add specialisation and return its name
              -- addSpecialisation name' etyp args' body'
              -- for inlining version, just return the specialised body
              addSpecialisation name' etyp args' body'
              return (EVar name')
    Nothing -> return e <* warn ["! specVar ", unName n, " to ", str etyp, " - NO res"]

specialiseExp e@(ECon n) etyp = specialiseCon n etyp

specialiseExp e@(EApp fun a) etyp = do
  (aps, atyp0) <- tiExpr a `wrapError` a `wrapError` e
  (fps, ftyp0) <- tiExpr fun `wrapError` fun `wrapError` e
  reduceContext aps
  reduceContext fps
  atyp <- withCurrentSubst atyp0
  ftyp <- withCurrentSubst ftyp0
  subst <- getCurrentSubst
  warn [ "> specApp (", str e
       , ") fun = ", str fun," : ", str ftyp
       , "; arg = ", str a, " : ", str (aps :=> atyp)
       , "; target: ", str etyp
       -- , "subst: ", str subst
       ]
  phi <- mgu ftyp (atyp :-> etyp) `wrapError` ("specialise", e)
  warn ["< mgu", str(ftyp, atyp :-> etyp), " = " , str phi]
  let atyp' = apply phi atyp
  let ftyp' = apply phi ftyp
  a' <- specialiseExp (apply phi a) atyp'
  f' <- specialiseExp (apply phi fun) ftyp'
  warn ["< specApp - fun = ",str (ETyped f' ftyp')," arg: ", str (ETyped a' atyp')]
  return (EApp f' a')

specialiseExp e@(ELam args body) etyp = withLocalEnv do
  let args' = attachTypes args (argTypes etyp)
  addArgs args'
  body' <- specialiseExp body (resultType etyp args)
  warn ["< specLam ", str e, " : ", str etyp, " ~>", str (ELam args' body')]
  return (ELam args' body')

specialiseExp e@(ETyped e' t) etyp = do
  e'' <- specialiseExp e' etyp
  return (ETyped e'' etyp)

specialiseExp (EBlock stmts) etyp = withLocalEnv do
  stmts' <- mapM (`specialiseStmt` etyp) stmts
  return (EBlock stmts')

-- this should never happen, but just in case:
specialiseExp e etyp = throwError("FAILED to specialise "++str e)


specialiseCon :: Name -> Ty -> TCM Expr
specialiseCon f t = do
  fscheme@(Forall tvs (fps :=> ftyp)) <- askType f
  phi <- mgu ftyp t
  let tvs' = apply phi (map TyVar tvs)
  let f' = specName f tvs'
  warn ["! specCon ",unName f," : ", str fscheme, " to ", unName f', " : ", str t]
  return (ECon f')

specName :: Name -> [Ty] -> Name
specName n [] = n
specName (Name n) ts = Name $ n ++ "<" ++ intercalate "," (map str ts) ++ ">"
-- alternatively we could use similar Unicode letter characters:
-- Canadian Syllabics Pa (U+1438), Po (U+1433), and Final Short Horizontal Stroke (U+1428)
-- specName n ts = n ++ "\x1438" ++ intercalate "\x1428" (map str ts) ++ "\x1433"
-- or mangle, e.g. using $ and underscore:
-- specName n ts = n ++ "$" ++ intercalate "_" (map str ts) ++ "$"

specialiseStmt :: Stmt a -> Ty -> TCM (Stmt a)
specialiseStmt (SExpr a e) _ = do
  (ps, t) <- tiExpr e `wrapError` e
  e' <- specialiseExp e t
  return (SExpr a e')
specialiseStmt (SAlloc a x t) _ = do
    extEnv x (monotype $ stackT t)
    return (SAlloc a x t)
