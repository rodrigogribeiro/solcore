module Solcore.Frontend.TypeInference.TcContract where 

import Control.Monad
import Control.Monad.Except

import Data.List

import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax
import Solcore.Frontend.TypeInference.NameSupply
import Solcore.Frontend.TypeInference.TcEnv
import Solcore.Frontend.TypeInference.TcMonad
import Solcore.Frontend.TypeInference.TcStmt
import Solcore.Frontend.TypeInference.TcSubst
import Solcore.Frontend.TypeInference.TcUnify
import Solcore.Primitives.Primitives


-- type inference for declarations

tcDecl :: Decl -> TcM ()
tcDecl (FieldDecl fd) = tcField fd
tcDecl (InstDecl id) = tcInstance id 
tcDecl d@(FunDecl _) = tcBindGroup [d]
tcDecl (MutualDecl ds) = tcBindGroup ds 
tcDecl (ConstrDecl cd) = tcConstructor cd 
tcDecl _ = return ()

tcField :: Field -> TcM ()
tcField d@(Field n t (Just e)) 
  = do
      -- FIXME: Should we return the constraints?
      (ps', t') <- tcExp e 
      s <- mgu t t' `wrapError` d 
      return () 
tcField (Field _ _ _) = return ()

tcInstance :: Instance -> TcM ()
tcInstance = undefined 

tcBindGroup :: [Decl] -> TcM ()
tcBindGroup = undefined 

tcConstructor :: Constructor -> TcM ()
tcConstructor (Constructor ps bd) 
  = do
      -- building parameters for constructors
      let f (Typed n t) = pure (n, monotype t)
          f (Untyped n) = ((n,) . monotype) <$> freshTyVar
      lctx <- mapM f ps 
      withLocalCtx lctx (mapM_ tcStmt bd) 
  
-- checking class definitions and adding them to environment 

checkClasses :: [Class] -> TcM ()
checkClasses = mapM_ checkClass 

checkClass :: Class -> TcM ()
checkClass (Class ps n vs v sigs) 
  = mapM_ checkSignature sigs 
    where
      checkSignature sig@(Signature f ctx ps mt)
        = do 
            pst <- mapM tyParam ps 
            unless (null ctx && v `elem` fv (funtype pst mt))
                   (throwError $ "invalid class declaration: " ++ unName n)
            addClassMethod (InCls n (TyVar v) (TyVar <$> vs))
                           sig 

addClassMethod :: Pred -> Signature -> TcM ()
addClassMethod p@(InCls _ _ _) (Signature f ctx ps t) 
  = do
      tps <- mapM tyParam ps  
      let ty = funtype tps t
          vs = fv ty
      extFunEnv f (Forall vs ([p] :=> ty))
addClassMethod p@(_ :~: _) (Signature n _ _ _) 
  = throwError $ unlines [
                    "Invalid constraint:"
                  , pretty p 
                  , "in class method:"
                  , unName n
                  ]

-- checking instances and adding them in the environment

checkInstances :: [Instance] -> TcM ()
checkInstances = mapM_ checkInstance 

checkInstance :: Instance -> TcM ()
checkInstance (Instance ctx n ts t funs)
  = do 
      let ipred = InCls n t ts
      -- checking the coverage condition 
      insts <- askInstEnv n `wrapError` ipred
      checkOverlap ipred insts
      coverage <- askCoverage
      when coverage (checkCoverage n ts t `wrapError` ipred)
      -- checking Patterson condition 
      checkMeasure ctx ipred `wrapError` ipred
      -- checking instance methods
      mapM_ (checkMethod ipred) funs
      let ninst = ctx :=> InCls n t ts 
      -- add to the environment 
      addInstance n ninst 

checkOverlap :: Pred -> [Inst] -> TcM ()
checkOverlap _ [] = pure ()
checkOverlap p@(InCls _ t _) (i:is) 
  = do 
        i' <- renameVars (fv t) i
        case i' of 
          (ps :=> (InCls _ t' _)) -> 
            case mgu t t' of
              Right _ -> throwError (unlines ["instance:"
                                             , pretty p
                                             , "with:"
                                             , pretty i'])
              Left _ -> checkOverlap p is
        return ()

checkCoverage :: Name -> [Ty] -> Ty -> TcM ()
checkCoverage cn ts t 
  = do 
      let strongTvs = fv t 
          weakTvs = fv ts 
          undetermined = weakTvs \\ strongTvs
      unless (null undetermined) $ 
          throwError (unlines [
            "Coverage condition fails for class:"
          , unName cn 
          , "- the type:"
          , pretty t 
          , "does not determine:"
          , intercalate ", " (map pretty undetermined)
          ])

checkMethod :: Pred -> FunDef -> TcM () 
checkMethod ih@(InCls n t ts) (FunDef sig bd) 
  = do
      cn <- askCurrentContract 
      st@(Forall _ (qs :=> _)) <- askFun cn (sigName sig)
      p <- maybeToTcM (unwords [ "Constraint for"
                               , unName n
                               , "not found in type of"
                               , unName $ sigName sig])
                      (findPred n qs)
      s <- liftEither (matchPred p ih) `wrapError` ih
      (qs' :=> ty') <- freshInst st 
      tps <- mapM tyParam (sigParams sig)
      let it = funtype tps (sigReturn sig)
      match it (apply s ty') `wrapError` ih 
      pure ()

tyParam :: Param -> TcM Ty 
tyParam (Typed _ t) = pure t 
tyParam (Untyped _) = freshTyVar

findPred :: Name -> [Pred] -> Maybe Pred 
findPred _ [] = Nothing 
findPred n (p@(InCls n' _ _) : ps) 
  | n == n' = Just p 
  | otherwise = findPred n ps

anfInstance :: Inst -> Inst
anfInstance inst@(q :=> p@(InCls c t [])) = inst
anfInstance inst@(q :=> p@(InCls c t as)) = q ++ q' :=> InCls c t bs 
  where
    q' = zipWith (:~:) bs as
    bs = map TyVar $ take (length as) freshNames
    tvs = fv inst
    freshNames = filter (not . flip elem tvs) (TVar <$> namePool)

-- checking Patterson conditions 

checkMeasure :: [Pred] -> Pred -> TcM ()
checkMeasure ps c 
  = if measure ps < measure c then return () 
    else throwError $ unlines [ "Instance "
                              , pretty c
                              , "does not satisfy the Patterson conditions."]


