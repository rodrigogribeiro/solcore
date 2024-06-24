module Solcore.Desugarer.Defunctionalizer where

import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Data.List

import Solcore.Frontend.Syntax.Contract
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.Stmt
import Solcore.Frontend.Syntax.Ty 


-- top level interface

defunctionalize :: CompUnit -> IO (Either String CompUnit)
defunctionalize = undefined 


-- Defunctionalization monad 

type DefunM a 
  = ReaderT String (ExceptT String 
                   (WriterT [Decl] 
                   (StateT Env IO))) a

type Env = ()

runCompilerM :: [Name] -> Env -> DefunM a -> IO (Either String a, [Decl])
runCompilerM ns env m 
  = evalStateT (runWriterT (runExceptT (runReaderT m (mkPrefix ns)))) env


-- type class for defunctionalization 

class Defun a where 
  defun :: a -> DefunM a  
  
instance Defun a => Defun [a] where 
  defun = mapM defun 

instance Defun a => Defun (Maybe a) where 
  defun Nothing = return Nothing 
  defun (Just x) = Just <$> defun x

instance Defun Decl where 
  defun (InstDecl inst)
    = InstDecl <$> defun inst 
  defun (FunDecl fun)
    = FunDecl <$> defun fun 
  defun (ConstrDecl con)
    = ConstrDecl <$> defun con 
  defun d = return d

instance Defun Instance where 
  defun (Instance ps n ts m funs)
    = Instance ps n ts m <$> defun funs 

instance Defun FunDef where 
  defun (FunDef n ret ps bd)
    = FunDef n ret ps <$> defun bd

instance Defun Constructor where 
  defun (Constructor ps bd)
    = Constructor ps <$> defun bd

instance Defun Stmt where
  defun (l := e) 
    = (l :=) <$> defun e
  defun (Let n mt me)
    = Let n mt <$> defun me
  defun (StmtExp e)
    = StmtExp <$> defun e 
  defun (Return e)
    = Return <$> defun e 
  defun (Match es eqns)
    = Match <$> defun es <*> defun eqns 

instance Defun Equation where 
  defun (ps, ss) = (ps,) <$> defun ss

instance Defun Exp where 
  defun (Con n es)
    = Con n <$> defun es 
  defun (FieldAccess e n)
    = flip FieldAccess n <$> defun e 
  defun e = return e

-- utilities functions 

mkPrefix :: [Name] -> String 
mkPrefix = intercalate "_" . map unName 


