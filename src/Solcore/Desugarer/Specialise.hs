module Solcore.Desugarer.Specialise(specialiseCompUnit) where
{- * Specialisation
Create specialised versions of polymorphic and overloaded (TODO) functions.
This is meant to be run on typed and defunctionalised code, so no higher-order functions.
-}
import Control.Monad
import Control.Monad.State
import Data.List(intercalate)
import qualified Data.Map as Map
import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax
import Solcore.Frontend.TypeInference.Id ( Id(..) )
import Solcore.Frontend.TypeInference.TcEnv(TcEnv(..),TypeInfo(..))
import Solcore.Frontend.TypeInference.TcSubst
import Solcore.Frontend.TypeInference.TcUnify
import Solcore.Primitives.Primitives
import System.Exit


-- ** Specialisation state and monad
-- SpecState and SM are meant to be local to this module.
type Table a = Map.Map Name a
emptyTable :: Table a
emptyTable = Map.empty

type TcFunDef = FunDef Id
type TcExp = Exp Id

type Resolution = (Ty, TcFunDef)
data SpecState = SpecState
  { spResTable :: Table [Resolution]
  , specTable :: Table TcFunDef
  , spTypeTable :: Table TypeInfo
  , spGlobalEnv :: TcEnv
  , splocalEnv :: Table Ty
  }

type SM a = StateT SpecState IO a
runSM :: SM a -> TcEnv -> IO a
runSM m env = evalStateT m (initSpecState env)

writeln :: MonadIO m => String -> m ()
writeln = liftIO  . putStrLn
writes :: MonadIO m => [String] -> m ()
writes = writeln . concat
errors = error . concat

panics :: MonadIO m => [String] -> m a
panics msgs = do
    writes ("PANIC: ":msgs)
    liftIO exitFailure

withLocalState :: SM a -> SM a
withLocalState m = do
    s <- get
    a <- m
    put s
    return a

initSpecState :: TcEnv -> SpecState
initSpecState env = SpecState
    { spResTable = emptyTable
    , specTable = emptyTable
    , spTypeTable = typeTable env
    , spGlobalEnv = env
    , splocalEnv = emptyTable
    }

addSpecialisation :: Name -> TcFunDef -> SM ()
addSpecialisation name fd = modify $ \s -> s { specTable = Map.insert name fd (specTable s) }

lookupSpecialisation :: Name -> SM (Maybe TcFunDef)
lookupSpecialisation name = gets (Map.lookup name . specTable)

addResolution :: Name -> Ty -> TcFunDef -> SM ()
addResolution name ty fun = do
    modify $ \s -> s { spResTable = Map.insertWith (++) name [(ty, fun)] (spResTable s) }

lookupResolution :: Name -> Ty ->  SM (Maybe (TcFunDef, Ty, Subst))
lookupResolution name ty = gets (Map.lookup name . spResTable) >>= findMatch ty where
  str :: Pretty a => a -> String
  str = pretty
  findMatch :: Ty -> Maybe [Resolution] -> SM (Maybe (TcFunDef, Ty, Subst))
  findMatch etyp (Just res) = firstMatch etyp res
  findMatch _ Nothing = return Nothing
  firstMatch :: Ty -> [Resolution] -> SM (Maybe (TcFunDef, Ty, Subst))
  firstMatch etyp [] = return Nothing
  firstMatch etyp ((t,e):rest)
    | Right subst <- mgu t etyp = do  -- TESTME: match is to weak for MPTC, but isn't mgu too strong?
        writes ["! lookupRes: match found: ", str t, " ~ ", str etyp, " => ", str subst]
        return (Just (e, t, subst))
    | otherwise = firstMatch etyp rest

specialiseCompUnit :: CompUnit Id -> TcEnv -> IO (CompUnit Id)
specialiseCompUnit compUnit env = flip runSM env do
    addGlobalResolutions compUnit
    contracts' <- forM (contracts compUnit) specialiseContract
    return $ compUnit { contracts = contracts' }

addGlobalResolutions :: CompUnit Id -> SM ()
addGlobalResolutions _ = return ()   -- TODO when global declarations are added

-------------------------------------------------------------------------------

specialiseContract :: Contract Id -> SM (Contract Id)
specialiseContract (Contract name args decls) = withLocalState do
    addContractResolutions (Contract name args decls)
    forM_ entries specEntry
    st <- gets specTable
    let decls' = map (FunDecl . snd) (Map.toList st)
    return $ Contract name args decls'
    where
      entries = ["main"]    -- Eventually all public methods

specEntry :: Name -> SM ()
specEntry name = do
    let any = TyVar (TVar (Name "any"))
    mres <- lookupResolution name any
    case mres of
      Just (fd, ty, subst) -> do
        writes ["resolution: ", show name, " : ", pretty ty, "@", pretty subst]
        specFunDef fd subst
        return ()
      Nothing -> do
        errors ["! specEntry: no resolution found for ", show name]

addContractResolutions :: Contract Id -> SM ()
addContractResolutions (Contract name args decls) = do
  forM_ decls addDeclResolution

addDeclResolution :: Decl Id -> SM ()
addDeclResolution (FunDecl fd) = do
  let sig = funSignature fd
  let name = sigName sig
  let funType = typeOfTcFunDef fd
  addResolution name funType fd
  writes ["! addDeclResolution: ", show name, " : ", pretty funType]
addDeclResolution (InstDecl inst) = writeln "WARN: Instance declaration not supported yet"
addDeclResolution _ = return ()

-- | `specExp` specialises an expression to given type
specExp :: TcExp -> Ty -> SM TcExp
specExp e@(Call Nothing i args) ty = do
  writes ["> specExp (Call): ", pretty e, " : ", pretty (idType i), " ~> ", pretty ty]
  (i', args') <- specCall i args ty
  let e' = Call Nothing i' args'
  writes ["< specExp (Call): ", pretty e']
  return e'

specExp e ty = do
  writes ["> specExp: ", pretty e, " : ", pretty (typeOfTcExp e), " ~> ", pretty ty]
  return e

-- | Specialise a function call
-- given actual arguments and the expected result type
specCall :: Id -> [TcExp] -> Ty -> SM (Id, [TcExp])
specCall i _ (TyVar _) = panics ["specCall ", pretty i, ": polymorphic result type"]
specCall i _ (_ :-> _) = panics ["specCall ", pretty i, ": function result type"]
specCall i args ty = do
  -- writes ["> specCall: ", show i, show args, " : ", pretty ty]
  let name = idName i
  let argTypes = map typeOfTcExp args
  let typedArgs = zip args argTypes
  args' <- forM typedArgs (uncurry specExp)
  let funType = foldr (:->) ty argTypes
  -- writes ["! specCall: ", show name, " : ", pretty funType]
  mres <- lookupResolution name funType
  case mres of
    Just (fd, ty, subst) -> do
      -- writes ["resolution: ", show name, " : ", pretty ty, "@", pretty subst]
      name' <- specFunDef fd subst
      -- writes ["< specCall: ", pretty name']
      return (Id name' ty, args')
    Nothing -> do
      writes ["! specCall: no resolution found for ", show name, " : ", pretty funType]
      return (i, args')

-- | `specFunDef` specialises a function definition
-- to the given type of the form `arg1Ty -> arg2Ty -> ... -> resultTy`
-- first lookup if a specialisation to the given type exists
-- if not, look for a resolution (definition matching the expected type)
-- create a new specialisation of it and record it in `specTable`
-- returns name of the specialised function
specFunDef :: TcFunDef -> Subst -> SM Name
specFunDef fd subst = do
  let sig = funSignature fd
  let name = sigName sig
  let funType = typeOfTcFunDef fd
  let tvs = fv funType
  let tvs' = apply subst (map TyVar tvs)
  let name' = specName name tvs'
  let ty' = apply subst funType
  mspec <- lookupSpecialisation name'
  case mspec of
    Just fd' -> return name'
    Nothing -> do
      let sig' = apply subst (funSignature fd)
      body' <- specBody subst (funDefBody fd)
      let fd' = FunDef sig'{sigName = name'} body'
      writes ["! specFunDef: adding specialisation ", show name', " : ", pretty ty']
      addSpecialisation name' fd'
      return name'

specBody :: Subst -> [Stmt Id] -> SM [Stmt Id]
specBody subst body = mapM (specStmt subst) body

specStmt :: Subst -> Stmt Id -> SM(Stmt Id)
specStmt subst stmt@(Return e) = do
  let ty = typeOfTcExp e
  let ty' = apply subst ty
  case ty' of
    TyVar _ -> panics ["specStmt(",pretty stmt,"): polymorphic return type"]
    _ :-> _ -> panics ["specStmt(",pretty stmt,"): function return type"]
    _ -> return ()
  writes ["> specExp (Return): ", pretty e," : ", pretty ty, " ~> ", pretty ty']
  e' <- specExp e ty'
  writes ["< specExp (Return): ", pretty e']
  return $ Return e'
specStmt subst stmt = errors ["specStmt not implemented for: ", show stmt]
-- specStmt subst stmt = pure stmt -- FIXME

specName :: Name -> [Ty] -> Name
specName n [] = n
specName n ts = Name $ show n ++ "$" ++ intercalate "_" (map pretty ts)

typeOfTcExp :: TcExp -> Ty
typeOfTcExp (Var i)               = idType i
typeOfTcExp (Con i _)             = idType i
typeOfTcExp (Lit (IntLit _))      = word --TyCon "Word" []
typeOfTcExp (Call Nothing i args) = idType i
typeOfTcExp (Lam args body)       = funtype tas tb where
  tb = typeOfTcBody body
  tas = map typeOfTcParam args
typeOfTcExp e = error $ "typeOfTcExp: " ++ show e

typeOfTcStmt :: Stmt Id -> Ty
typeOfTcStmt (n := e) = unit
typeOfTcStmt (Let n _ _) = idType n
typeOfTcStmt (StmtExp e) = typeOfTcExp e
typeOfTcStmt (Return e) = typeOfTcExp e
typeOfTcStmt (Match _ ((pat, body):_)) = typeOfTcBody body

typeOfTcBody :: [Stmt Id] -> Ty
typeOfTcBody []    = unit
typeOfTcBody [s]   = typeOfTcStmt s
typeOfTcBody (_:b) = typeOfTcBody b

typeOfTcParam :: Param Id -> Ty
typeOfTcParam (Typed i t)  = idType i  -- seems better than t - see issue #6
typeOfTcParam (Untyped i) = idType i

typeOfTcSignature :: Signature Id -> Ty
typeOfTcSignature sig = funtype (map typeOfTcParam $ sigParams sig) (returnType sig) where
  returnType sig = case sigReturn sig of
    Just t -> t
    Nothing -> error ("no return type in signature of: " ++ show (sigName sig))

typeOfTcFunDef :: TcFunDef -> Ty
typeOfTcFunDef (FunDef sig _) = typeOfTcSignature sig
