module Solcore.Frontend.TypeInference.Checker where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State 

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map 

import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.Ty
import Solcore.Frontend.TypeInference.Constraints
import Solcore.Frontend.TypeInference.ISyntax
import Solcore.Frontend.TypeInference.NameSupply
import Solcore.Frontend.TypeInference.TcMonad


tiExpr :: Expr -> TCM ([Pred], Ty)
tiExpr (EInt _) = pure ([], TInt)
-- check a lambda (function) such as `\x y -> x`
tiExpr (ELam args e1) = do
  env <- getEnv
  as <- addArgs args
  (ps, t1) <- tiExpr e1
  -- typing function body may have created constraints on argument types
  -- these are recorded in the current substitution, which needs to be applied here
  tas <- withCurrentSubst as
  putEnv env
  pure $ (ps, foldr (:->) t1 tas)

tiExpr (EVar v) = do
  s <- askType v
  ps :=> t <- freshInst s
  pure (ps, t)

tiExpr (ECon v) = do
  s <- askType v
  ps :=> t <- freshInst s
  pure (ps, t)

-- check an application (function call)
tiExpr e@(EApp fun arg) = do
  (ps, t1) <- tiExpr fun
  (qs, t2) <- tiExpr arg
  freshname <- tcmDeplete
  let tr = TyVar freshname
  unify t1 (t2 :-> tr) `wrapError` e
  s <- getSubst
  let tr' = apply s tr
  let preds = apply s (ps ++ qs)
  -- info ["< EApp ", str fun, ":", str t1, "@", str arg,  " : ", str (preds :=> tr')]
  return (preds, tr')

tiExpr exp@(ELet x e1 e2) = withLocalEnv $ do
  tiBindgroup [ValBind x [] e1]
  (qs, t) <- tiExpr e2
  return (qs, t)

tiExpr (ETyped e t) = do
  (ps, t1) <- tiExpr e
  unify t t1
  return (ps, t1)

tiExpr (EBlock stmts) = withLocalEnv (go stmts) where
    unit = ([], unitT)
    go [] = return unit
    go [stmt] = tiStmt stmt
    go (stmt:rest) = do -- tiStmt stmt >> go rest
      tiStmt stmt
      go rest
    tiStmt :: ToStr ann => Stmt ann -> TCM ([Pred], Ty)
    tiStmt (SExpr ann e) = do
      localEnv <- askTypes (freeVars e)
      warn [str ann, " ~> ", str e]
      (ps, t) <- tiExpr e `wrapError` e
      Forall as (ps1 :=> t1) <- generalize (ps, t) `wrapError` e
      info [showSmallEnv localEnv, " |- ", str e, " : ", str $ ps1 :=> t1]
      return unit

    tiStmt (SAlloc ann n t) = do
      extEnv n (monotype $ stackT t)
      q <- askType n
      let n' = unName n
      info ["alloc ", n', " : ", str t, " ~> ", n', " : ", str q]
      return unit

tiExpr (ECase e alts) = do -- TODO unify scrutinised expr type with con target
  targetType <- TyVar <$> tcmDeplete
  (ps, scrutType) <- tiExpr e   -- scrutinisied expresion
  ps' <- tiAlts scrutType targetType alts
  resultType <- withCurrentSubst targetType
  return (ps ++ ps', resultType)

tiAlts ::Ty -> Ty -> [CaseAlt] -> TCM [Pred]
tiAlts scrut target [] = return []
tiAlts scrut target (alt:alts) = do
  warn ["> tiAlts, target : ", str target]
  (ps, expected) <- tiAlt target alt
  unify scrut expected `wrapError` alt
  target' <- withCurrentSubst target
  scrut' <- withCurrentSubst scrut
  ps' <- tiAlts scrut' target' alts
  return (ps ++ ps')


tiAlt :: Ty -> CaseAlt -> TCM ([Pred], Ty)  -- result to be unified with scrutType
tiAlt target alt@(CaseAlt con args e) = withLocalEnv $ do
  warn ["> tiAlt ", str alt, " : ", str target]
  conScheme <- askType con
  (qs :=> conType) <- freshInst conScheme
  (argTypes, conResultType) <- addConArgs conType args
  warn ["! tiAlt ", unName con, ":", str conType, " ", str argTypes]
  (ps,t) <- tiExpr e
  unify target t `wrapError` alt
  withCurrentSubst (ps, conResultType)

addConArgs :: Ty -> [Arg] -> TCM ([Ty], Ty)
addConArgs conType [] = return ([], conType)
addConArgs (at :-> rest) (arg:args) = do
  let name = argName arg
  extEnv name (monotype at)
  (ats, rt) <- addConArgs rest args
  return (at:ats, rt)

------------------------------------------------------------

tiBind :: Bind -> TCM ([Pred], Ty)
tiBind (Bind n args e) = withLocalEnv $ do
  as <- addArgs args
  (ps0, t0) <- tiExpr e
  let t1 = foldr (:->) t0 as
  -- Unify the inferred type with the assumed type variable
  assumed <- askType n
  (aq :=> at) <- freshInst assumed
  unify at t1
  t <- withCurrentSubst t1
  ps <- withCurrentSubst ps0
  warn ["< tiBind ", unName n, " : ", str (ps :=> t)]
  return (ps, t)


tiArg :: Arg -> TCM (Name, Ty)
tiArg (UArg name) = do
  a <- tcmDeplete
  pure (name, TyVar a)
tiArg (TArg name t) = pure (name, t)

addArgs :: [Arg] -> TCM [Ty]
addArgs args = do
  typedArgs <- forM args tiArg
  forM typedArgs $ \(n,t) -> extEnv n (monotype t) >> pure t

generalize :: ([Pred], Ty) -> TCM Scheme
generalize (ps0, t0) = do
  envVars <- getFreeEnvVars
  (ps1, t1) <- withCurrentSubst (ps0, t0)
  ce <- gets tcsCT
  ps2 <- reduceContext ps1
  -- reduceContext may have extended the subst, need to reapply it
  t2 <- withCurrentSubst t1
  let typeVars =  ftv (ps2, t2)
  let scheme = Forall (typeVars \\ envVars) (ps2 :=> t2)
  info ["< generalize: ", str (legibleScheme scheme)]
  return scheme

schemeOf :: Expr -> TCM Scheme
schemeOf exp = wrapError ty exp where
  ty = tiExpr exp >>= generalize

tiDecl :: Decl -> TCM ()
tiDecl (ValDecl n qt) = do
  let tvs = ftv qt
  let s = Forall tvs qt
  extEnv n s

tiDecl d@(ValBind n as e) = do
  tiBindgroup [d]
  Forall tvs (qs :=> typ) <- askType n
  let exp = formLambda as typ e
  addResolution n typ exp

tiDecl (Mutual ds) = tiBindgroup ds

-- check type declaration,such as `Option a = None |  Some a`
tiDecl (TypeDecl typ@(TyCon name args) alts) = do
  constructors <- tiConAlts typ alts
  forM_ constructors addCon
  let consNames = map fst constructors
  let arity = length args
  let typeInfo = (arity, consNames)
  modify (addTypeInfo name typeInfo)
  where
      addCon (name, typ) = extEnv name typ

-- check instance declaration such as `instance Int : Eq`
tiDecl (InstDecl qp methods) = tiInstance qp methods

-- check class declaration such as `class a:Eq { eq : a -> a -> Bool }`
tiDecl(ClsDecl pred@(InCls c (TyVar mainTVar) as) methods) = do
  methodNames <- forM methods tiMD
  let classInfo = (className, methodNames)
  modify (addClassInfo className (classArity, methodNames)) where
    className = predName pred
    classArity = length $ predParams pred
    tiMD (ValDecl name ([] :=> typ)) = do
      -- ambiguity check
      unless (mainTVar `elem` ftv typ) $ throwError $ unlines
        [ "- in the declaration of class " ++ unName className ++ ":"
        , "  ambiguous type variable in method " ++ unName name ++ ": " ++ show typ
        ]
      extEnv name $ Forall (ftv typ) ([pred] :=> typ)
      return name
    tiMD (ValDecl name (preds :=> typ)) = throwError $ unlines
           [ "- in the declaration of class" ++ unName (predName pred)
           , "- in method " ++ unName name ++ ":"
           , "  qualifiers not allowed in method types"
           ]

    tiMD decl = throwError $ unlines
           [ "- in the declaration of class " ++ unName className ++ ":"
           , "  only type declarations allowed, illegal declaration: " ++ show decl
           ]
tiDecl(ClsDecl (InCls c as complexType) _) = throwError $ unlines
           [ "- in the declaration of class " ++ unName c ++ ":"
           , "  main argument must be a type variable, illegal type: " ++ show complexType
           ]
tiDecl (Pragma prag) = process prag where
    process "log" = void $ setLogging True >> warn ["-- Logging ON  --"]
    process "nolog" = setLogging False >> warn ["-- Logging OFF --"]
    process "nocoverage" = setCoverageCheck False >> warn ["-- Coverage Checking OFF --"]
    process "coverage" = setCoverageCheck True >> warn ["-- Coverage Checking OFF --"]

tiConAlts :: Ty -> [ConAlt] -> TCM [(Name, Scheme)]
tiConAlts typ alts = forM alts (tiConAlt typ)

-- check a constructor alternative such as `Some a`
-- and within definition of `Option a`, give it type `forall a.a -> Option a`
tiConAlt :: Ty -> ConAlt -> TCM (Name, Scheme)
tiConAlt result (ConAlt cname argumentTypes) = pure (cname, simpleGen constructorType) where
  constructorType = foldr (:->) result argumentTypes  -- at1 :-> at2 :-> ... :-> result
  simpleGen :: Ty -> Scheme
  simpleGen t = Forall (ftv t) ([] :=> t)

tiInstance :: Qual Pred -> [Decl] -> TCM ()
tiInstance inst@(constraint :=> ihead@(InCls c t as)) methods = do
  let header = InstDecl inst [] -- for error messages
  -- warn ["+ tiInstance ", str inst]
  ois <- getInsts c `wrapError` header
  checkOverlap t ois
  enabled <- gets tcsCoverageEnabled
  when enabled $ checkCoverage (unName c) as t `wrapError` header
  checkMeasure constraint ihead `wrapError` header
  forM_ methods (checkMethod ihead)
  let anf = anfInstance inst
  modify (addInstInfo anf)
  where
    checkMeasure :: [Pred] -> Pred -> TCM ()
    checkMeasure constraint ihead =
      if measure constraint < measure ihead then return ()
      else throwError $ unwords ["size of constraints must smaller than the head"]

    checkOverlap :: Ty -> [Inst] -> TCM ()
    checkOverlap t [] = pure ()
    checkOverlap t (oi:is) = do
      -- Type variables in an instance declaration are implicitly bound
      -- so they need to be renamed before the overlap check
      -- doing this here rather than in tiInstance for better error msgs
      oi' <- renameFreeVars oi
      case oi' of
        (_ :=> InCls _ u _) -> case mgu t u of
          Right s -> throwError
                 (unwords ["instance",str inst,"overlaps", str oi])
          Left _ -> checkOverlap t is
        (_ :=> (t1 :~: t2)) ->
          error(unwords["internal error: illegal instance", str oi])


    checkCoverage :: String -> [Ty] -> Ty -> TCM ()
    checkCoverage c as t = do
      let strongTVs = ftv t
      let weakTVs = ftv as
      let undetermined = weakTVs \\ strongTVs
      unless (null undetermined) $ do
        let undetermined_str = intercalate ", " ((unName . unVar) <$> undetermined)
        throwError $ unwords
          [ "Coverage condition fails in class", c
          , "- the type", str t, "does not determine"
          , undetermined_str
          ]

    findPred :: Name -> [Pred] -> Maybe Pred
    findPred cname (p:ps) | predName p == cname = Just p
                          | otherwise = findPred cname ps
    findPred cname [] = Nothing
    checkMethod :: Pred -> Decl -> TCM ()
    checkMethod ihead (ValBind name args body) = do
      let InCls cname cargs mainType = ihead
      Forall tvs (qs :=> genType)  <- askType name
      p <- maybeToTCM ("Constraint for "++ unName cname ++ 
                       " not found in type of "++ unName name)
                      (findPred cname qs)
      subst <- liftEither (matchPred p ihead) `wrapError` ihead
      let expType = apply subst genType
      let args' = apply subst args
      let exp = formLambda args' expType body
      let iTypes = apply subst (map TyVar tvs)
      (iq, it) <- tiExpr exp
      warn ["< tiExpr ", str exp, " : ", str (iq:=>it)]
      match it expType `wrapError` exp
      addResolution name expType exp
      return ()

formLambda :: [Arg] -> Ty -> Expr -> Expr
formLambda [] typ body = body
formLambda as typ body = ELam as body where
  addTypes [] t = []
  addTypes (a:as) (t :-> u)  = TArg (argName a) t : addTypes as u
  addTypes _ _ = error "formLambda: wrong number of arguments"

tiProg :: Prog -> TCM ()
tiProg (Prog decls) = do
  initContext decls 
  cleanTiDecls decls
  where
    cleanTiDecls = mapM_ cleanTiDecl
    cleanTiDecl d = clearSubst >> tiDecl d

initContext :: [Decl] -> TCM () 
initContext 
  = mapM_ insertContext 

insertContext :: Decl -> TCM ()
insertContext (ValDecl n (_ :=> t)) 
  = extEnv n (monotype t)
insertContext _ = return () 

tiBindgroup :: [Decl] -> TCM ()   -- TODO: specialisation
tiBindgroup binds = do
  binds <- scanDecls binds
  -- warn ["> tiBindgroup ", str binds]
  -- cannot clean substitution when doing mutual recursion
  qts <- mapM tiBind binds
  qts' <- withCurrentSubst qts
  schemes <- mapM generalize qts'
  let names = map bindName binds
  let results = zip names schemes
  mapM_ (uncurry extEnv) results
  where
    scanDecls :: [Decl] -> TCM [Bind]
    scanDecls [] = return []
    scanDecls (ValDecl n qt:ValBind n' as e:ds) | n == n' = do
    -- explicitly typed binding, add declared type to the environment
      let tvs = ftv qt
      let s = Forall tvs qt
      extEnv n s
      bs <- scanDecls ds
      return (Bind n as e:bs)
    scanDecls (ValBind n as e:ds) = do
    -- untyped binding, add fresh type variable to the environment
      a <- TyVar <$> tcmDeplete
      extEnv n (monotype a)
      bs <- scanDecls ds
      return (Bind n as e:bs)
    scanDecls (d:ds) = throwError ("illegal declaration in mutual: " ++ show d)

buildTLD :: [Decl] -> TCM TLDict
buildTLD decls = do
  tld <- foldM addTLD Map.empty decls
  modify (\st -> st { tcsTLD = tld })
  return tld
   where
    addTLD :: TLDict -> Decl -> TCM TLDict
    addTLD tld (ValBind name args body) = do
      scheme@(Forall tvs (ps :=> typ)) <- askType name
      let typedArgs = attachTypes args (argTypes typ)
      let def = (name, scheme, typedArgs, body)
      return (Map.insert name def tld)
    addTLD tld _ = return tld

attachTypes :: [Arg] -> [Ty] -> [Arg]
attachTypes = zipWith typedArg where
    typedArg (UArg name) t   = TArg name t
    typedArg (TArg name _) t = TArg name t


typeOfScheme :: Scheme -> Maybe Ty
typeOfScheme (Forall [] ([] :=> t)) = Just t
typeOfScheme _ = Nothing


---- Classes

-- Administraive Normal Form of an MPTC instance:
-- transform `instance C [as] t` into `[bs ~ as] => C bs t`
anfInstance :: Inst -> Inst
anfInstance inst@(q :=> p@(InCls c t [])) = inst
anfInstance inst@(q :=> p@(InCls c t as)) = (q ++ q') :=> InCls c t bs where
    q' = zipWith (:~:) bs as
    bs = map TyVar $ take (length as) freshNames
    tvs = ftv inst
    freshNames = filter (not . flip elem tvs) ((TVar . Name) <$> namePool)

insts :: InstTable -> Name -> [Inst]
insts ce n = Map.findWithDefault (error msg) n ce where
  msg = "Internal error: instance " ++ unName n ++ " not found"

getInsts :: Name -> TCM [Inst]
getInsts n = do
  ce <- gets tcsIT
  case Map.lookup n ce of
    Just is -> pure is
    Nothing -> throwError$ "unknown class: " ++  unName n

-- Transform equality constraints (such as a ~ Memory[Int]) into substitution
-- so that [b1:Ref[Int],b1 ~ Memory[Int]] becomes ([Memory[Int]:Ref[Int], b1 +-> Int)
simplifyEqualities :: [Pred] -> TCM [Pred]
simplifyEqualities ps = go [] ps where
    go rs [] = return rs
    go rs ((t :~: u):ps) = do
      phi <- mgu t u
      extSubst phi
      ps' <- withCurrentSubst ps
      rs' <- withCurrentSubst rs
      go rs' ps'
    go rs (p:ps) = go (p:rs) ps

byInstM :: InstTable -> Pred -> Maybe ([Pred], Subst)
byInstM ce p@(InCls i as t) = msum [tryInst it | it <- insts ce i] where
    tryInst :: Qual Pred -> Maybe ([Pred], Subst)
    tryInst c@(ps :=> h) =
        case matchPred h p of
          Left _ -> Nothing
          Right u -> let tvs = ftv h
                     in  Just (map (apply u) ps, expel tvs u)

-- Reducing contexts to hnf (head-normal form)
-- e.g. (Pair[Int, b] : Eq) ~> (Int:Eq, b:Eq) ~> (b:Eq)

toHnf :: Int -> Pred -> TCM [Pred]
toHnf _ (t :~: u) = do
  subst1 <- mgu t u
  extSubst subst1
  return []

toHnf depth pred
  | inHnf pred = return [pred]
  | otherwise = do
      ce <- gets tcsIT
      case byInstM ce pred of
        Nothing -> throwError ("no instance of " ++ str pred
                  ++"\nKnown instances:\n"++str ce)
        Just (preds, subst') -> do
            extSubst subst'
            toHnfs (depth-1) preds

toHnfs :: Int -> [Pred] -> TCM [Pred]
toHnfs depth ps = do
  subst <- getCurrentSubst
  ps2 <- simplifyEqualities ps >>= withCurrentSubst
  toHnfs' depth ps2

toHnfs' _ [] = return []
toHnfs' 0 ps = throwError("Max context reduction depth exceeded")
toHnfs' d preds@(p:ps) = do
  let d' = d-1
  rs1 <- toHnf d' p
  ps' <- withCurrentSubst ps   -- important, toHnf may have extended the subst
  rs2 <- toHnfs' d' ps'
  return (rs1 ++ rs2)


inHnf :: Pred -> Bool
inHnf (InCls c t args) = hnf t where
  hnf (TyVar _) = True
  hnf (TyCon _ _) = False
inHnf (_ :~: _) = False


reduceContext :: [Pred] -> TCM [Pred]
reduceContext preds = do
  let depth = maxReductionDepth
  unless (null preds) $ info ["> reduceContext ", str preds]
  ps1 <- toHnfs depth preds `wrapError` preds
  ps2 <- withCurrentSubst ps1
  unless (null preds) $ info ["< reduceContext ", str ps2]
  return (nub ps2)

maxReductionDepth :: Int
maxReductionDepth = 100
