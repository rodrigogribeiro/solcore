module Solcore.Frontend.Syntax.Contract where

import Data.Generics (Data, Typeable)

import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.Stmt
import Solcore.Frontend.Syntax.Ty 

-- compilation unit 

data CompUnit a
  = CompUnit {
      imports :: [Import]
    , contracts :: [Contract a]
    } deriving (Eq, Ord, Show, Data, Typeable)

newtype Import 
  = Import { unImport :: QualName }
    deriving (Eq, Ord, Show, Data, Typeable)
    
-- definition of the contract structure 

data Contract a
  = Contract {
      name :: Name
    , tyParams :: [Tyvar]
    , decls :: [Decl a]
    } deriving (Eq, Ord, Show, Data, Typeable)

-- definition of a algebraic data type 

data DataTy 
  = DataTy {
      dataName :: Name 
    , dataParams :: [Tyvar]
    , dataConstrs :: [Constr]
    } deriving (Eq, Ord, Show, Data, Typeable)

data Constr 
  = Constr {
      constrName :: Name 
    , constrTy :: [Ty]
    } deriving (Eq, Ord, Show, Data, Typeable)

-- definition of contract constructor 

data Constructor a 
  = Constructor {
      constrParams :: [Param a]
    , constrBody :: (Body a)
    } deriving (Eq, Ord, Show, Data, Typeable)
-- definition of a synonym 

data TySym 
  = TySym {
      symName :: Name 
    , varList :: [Tyvar]
    , symRhs :: Ty 
    } deriving (Eq, Ord, Show, Data, Typeable)

-- definition of classes and instances 

data Class a 
  = Class {
      classContext :: [Pred]
    , className :: Name 
    , paramsVar :: [Tyvar]
    , mainVar :: Tyvar 
    , signatures :: [Signature a]
    } deriving (Eq, Ord, Show, Data, Typeable)

data Signature a 
  = Signature {
      sigName :: Name
    , sigContext :: [Pred]
    , sigParams :: [Param a]
    , sigReturn :: Maybe Ty 
    } deriving (Eq, Ord, Show, Data, Typeable)

data Instance a 
  = Instance {
      instContext :: [Pred]
    , instName :: Name 
    , paramsTy :: [Ty]
    , mainTy :: Ty
    , instFunctions :: [FunDef a]
    } deriving (Eq, Ord, Show, Data, Typeable)

-- definition of contract field variables 

data Field a
  = Field {
      fieldName :: Name 
    , fieldTy :: Ty 
    , fieldInit :: Maybe (Exp a)
    } deriving (Eq, Ord, Show, Data, Typeable)

-- definition of functions 

data FunDef a
  = FunDef {
      funSignature :: Signature a 
    , funDefBody :: Body a 
    } deriving (Eq, Ord, Show, Data, Typeable)

data Decl a 
  = DataDecl DataTy 
  | SymDecl TySym
  | ClassDecl (Class a)
  | InstDecl (Instance a)
  | FieldDecl (Field a)
  | FunDecl (FunDef a)
  | MutualDecl [Decl a] -- used only after SCC analysis
  | ConstrDecl (Constructor a)
    deriving (Eq, Ord,Show, Data, Typeable)
