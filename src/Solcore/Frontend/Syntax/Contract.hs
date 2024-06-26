module Solcore.Frontend.Syntax.Contract where

import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.Stmt
import Solcore.Frontend.Syntax.Ty 

-- compilation unit 

data CompUnit 
  = CompUnit {
      imports :: [Import]
    , contracts :: [Contract]
    } deriving (Eq, Ord, Show)

newtype Import 
  = Import { unImport :: QualName }
    deriving (Eq, Ord, Show)
    
-- definition of the contract structure 

data Contract 
  = Contract {
      name :: Name
    , tyParams :: [Tyvar]
    , decls :: [Decl]
    } deriving (Eq, Ord, Show)

-- definition of a algebraic data type 

data DataTy 
  = DataTy {
      dataName :: Name 
    , dataParams :: [Tyvar]
    , dataConstrs :: [Constr]
    } deriving (Eq, Ord, Show)

data Constr 
  = Constr {
      constrName :: Name 
    , constrTy :: [Ty]
    } deriving (Eq, Ord, Show)

-- definition of contract constructor 

data Constructor 
  = Constructor {
      constrParams :: [Param]
    , constrBody :: Body 
    } deriving (Eq, Ord, Show)

-- definition of a synonym 

data TySym 
  = TySym {
      symName :: Name 
    , varList :: [Tyvar]
    , symRhs :: Ty 
    } deriving (Eq, Ord, Show)

-- definition of classes and instances 

data Class 
  = Class {
      classContext :: [Pred]
    , className :: Name 
    , paramsVar :: [Tyvar]
    , mainVar :: Tyvar 
    , signatures :: [Signature]
    } deriving (Eq, Ord, Show)

data Signature 
  = Signature {
      sigName :: Name
    , sigContext :: [Pred]
    , sigParams :: [Param]
    , sigReturn :: Ty 
    } deriving (Eq, Ord, Show)

data Instance 
  = Instance {
      instContext :: [Pred]
    , instName :: Name 
    , paramsTy :: [Ty]
    , mainTy :: Ty
    , instFunctions :: [FunDef]
    } deriving (Eq, Ord, Show)

-- definition of contract field variables 

data Field 
  = Field {
      fieldName :: Name 
    , fieldTy :: Ty 
    , fieldInit :: Maybe Exp 
    } deriving (Eq, Ord, Show)

-- definition of functions 

data FunDef 
  = FunDef {
      funSignature :: Signature 
    , funDefBody :: Body 
    } deriving (Eq, Ord, Show)

type Param = (Name, Ty)

data Decl 
  = DataDecl DataTy 
  | SymDecl TySym
  | ClassDecl Class
  | InstDecl Instance 
  | FieldDecl Field 
  | FunDecl FunDef
  | MutualDecl [Decl] -- used only after SCC analysis
  | ConstrDecl Constructor
    deriving (Eq, Ord,Show)
