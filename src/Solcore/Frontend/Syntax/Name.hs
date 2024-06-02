{-# LANGUAGE OverloadedStrings #-}
module Solcore.Frontend.Syntax.Name where 

import Data.String 

newtype Name 
  = Name {unName :: String}
    deriving (Eq, Ord)

instance Show Name where 
  show = unName

instance IsString Name where 
  fromString = Name

newtype QualName 
  = QualName { unQName :: [Name] }
    deriving (Eq, Ord, Show)
