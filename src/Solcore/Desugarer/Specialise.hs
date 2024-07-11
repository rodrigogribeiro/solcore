module Solcore.Desugarer.Specialise(specialiseCompUnit) where
import Control.Monad
import Control.Monad.State
import qualified Data.Map as Map
import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax
import Solcore.Frontend.TypeInference.Id ( Id(idType) )
import Solcore.Frontend.TypeInference.TcEnv(TcEnv(..),TypeInfo(..))
import Solcore.Frontend.TypeInference.TcSubst
import Solcore.Frontend.TypeInference.TcUnify
import Solcore.Primitives.Primitives

-- SpecState and SM are meant to be local to this module.
type Table a = Map.Map Name a
type TcFunDef = FunDef Id
type TcExp = Exp Id

type Resolution = (Ty, TcFunDef)
data SpecState = SpecState
  { spResTable :: Table [Resolution]
  , specTable :: Table TcFunDef
  , spTypeTable :: Table TypeInfo
  }

type SM a = StateT SpecState IO a
runSM :: SM a -> TcEnv -> IO a
runSM m env = evalStateT m (initSpecState env)

writeln :: MonadIO m => String -> m ()
writeln = liftIO  . putStrLn
writes :: MonadIO m => [String] -> m ()
writes = writeln . concat

initSpecState :: TcEnv -> SpecState
initSpecState env = SpecState
    { spResTable = Map.empty
    , specTable = Map.empty
    , spTypeTable = typeTable env
    }

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
specialiseCompUnit (CompUnit imports contracts) env = do
    contracts' <- forM contracts (\c -> runSM (specialiseContract c) env)
    return $ CompUnit imports contracts'

specialiseContract :: Contract Id -> SM (Contract Id)
specialiseContract (Contract name args decls) = do
    forM_ decls processDecl
    return $ Contract name args decls
    where
      processDecl(FunDecl fd@(FunDef sig body)) = do
        let name = show $ sigName sig
        let params = sigParams sig
        let result = sigReturn sig
        let s = unwords
        let funType = typeOfTcFunDef fd
        writes [name, " : ", pretty funType]
        -- writes [name, " : ", show params, show result]
        -- liftIO $ print body
      processDecl _ = pure ()


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
typeOfTcParam (Typed _ t) = t
typeOfTcParam (Untyped i) = idType i

typeOfTcSignature :: Signature Id -> Ty
typeOfTcSignature sig = funtype (map typeOfTcParam $ sigParams sig) (returnType sig) where
  returnType sig = case sigReturn sig of
    Just t -> t
    Nothing -> error ("no return type in signature of: " ++ show (sigName sig))

typeOfTcFunDef :: TcFunDef -> Ty
typeOfTcFunDef (FunDef sig _) = typeOfTcSignature sig
