module Solcore.Desugarer.MatchCompiler where

import Control.Monad.Identity 
import Control.Monad.Reader 
import Control.Monad.State 
import Control.Monad.Writer 

import Data.List

import Solcore.Frontend.Syntax.Contract
import Solcore.Frontend.Syntax.Stmt 
import Solcore.Frontend.Syntax.Name

-- top level interface for the compiler 

type Equations = [(Pat, [Stmt])]

matchCompiler :: [Name] -> [Exp] -> Equations -> (Equations, [FunDef]) 
matchCompiler ns es eqns 
  = undefined -- runCompilerM ns (matchCompilerM es eqns)

-- Compiler monad infra 

type CompilerM a 
  = ReaderT String (WriterT [FunDef] (StateT Int Identity)) a 

mkPrefix :: [Name] -> String 
mkPrefix = intercalate "_" . map unName 

inc :: CompilerM Int 
inc = do 
  i <- get 
  put (i + 1)
  return i

runCompilerM :: [Name] -> CompilerM a -> (a, [FunDef])
runCompilerM ns m
  = runIdentity $ evalStateT (runWriterT (runReaderT m (mkPrefix ns))) 0


matchCompilerM :: [Exp] -> Equations -> CompilerM Exp 
-- first case: No remaining equations. We generate an error.
matchCompilerM _ [] = defaultCase 
matchCompilerM _ _  = undefined 


defaultCase :: CompilerM Exp  
defaultCase = undefined


