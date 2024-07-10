module Solcore.Desugarer.Specialise(specialiseCompUnit) where
import Control.Monad
import Control.Monad.State
import qualified Data.Map as Map
import Solcore.Frontend.Syntax
import Solcore.Frontend.TypeInference.Id
import Solcore.Frontend.TypeInference.TcEnv(TcEnv(..),TypeInfo(..))

-- SpecState and SM are meant to be local to this module.
type Table a = Map.Map Name a
type TcFunDef = FunDef Id
type TcExp = Exp Id

type Resolution = (Ty, TcFunDef)
data SpecState = SpecState 
  { spResTable :: Table Resolution
  , specTable :: Table TcFunDef
  , spTypeTable :: Table TypeInfo
  }

type SM a = StateT SpecState IO a
runSM :: SM a -> TcEnv -> IO a
runSM m env = evalStateT m (initSpecState env)

initSpecState :: TcEnv -> SpecState
initSpecState env = SpecState 
    { spResTable = Map.empty
    , specTable = Map.empty
    , spTypeTable = typeTable env
    }

specialiseCompUnit :: CompUnit Id -> TcEnv -> IO (CompUnit Id)
specialiseCompUnit (CompUnit imports contracts) env = do
    contracts' <- forM contracts (\c -> runSM (specialiseContract c) env)
    return $ CompUnit imports contracts'

specialiseContract :: Contract Id -> SM (Contract Id)
specialiseContract (Contract name args decls) = do
    forM_ decls processDecl 
    return $ Contract name args decls
    where
      processDecl(FunDecl (FunDef sig body)) = do
        let name = show $ sigName sig
        let params = sigParams sig
        let result = sigReturn sig
        let s = unwords[name, ":", show params, show result]
        liftIO $ putStrLn s
        liftIO $ print body
      processDecl _ = pure ()


typeOfTcExp :: TcExp -> SM Ty
typeOfTcExp (Var i) = pure(idType i)
-- typeOfTcExp (Con i _) = idType i
typeOfTcExp (Lit (IntLit _)) = pure (TyCon "Word" [])
-- typeOfTcExp (Call )