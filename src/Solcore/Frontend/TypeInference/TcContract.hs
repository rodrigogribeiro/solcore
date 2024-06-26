module Solcore.Frontend.TypeInference.TcContract where 

import Control.Monad
import Control.Monad.Except

import Data.List

import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax
import Solcore.Frontend.TypeInference.TcMonad
import Solcore.Frontend.TypeInference.TcStmt
import Solcore.Frontend.TypeInference.TcSubst
import Solcore.Frontend.TypeInference.TcUnify
import Solcore.Primitives.Primitives

tcDecl :: Decl -> TcM ()
cDecl (FieldDecl fd) = tcField fd
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
tcConstructor = undefined
