module Main where

import Data.Map (Map)
import qualified Data.Map as Map

import Solcore.Desugarer.Defunctionalization
import Solcore.Frontend.TypeInference.Id
import Solcore.Frontend.Syntax
import Solcore.Primitives.Primitives 

import Test.Tasty 
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests 

tests :: TestTree 
tests 
  = testGroup "Tests" 
              [
                defunctionalization
              ]

defunctionalization :: TestTree 
defunctionalization 
  = testGroup "Defunctionalization tests"
              [
                testCase "collectArgs test" $ 
                  collectArgs functionName [filterLam1, filterLam2] 
                    @?= lamDefMap
              , testCase "createDataTy test" $ 
                  map (createDataTy contractName) (Map.toList lamDefMap) 
                    @?= [dt]
              ]


-- test inputs for creating data types for defunctionalization

contractName :: Name 
contractName = Name "TestContract"

functionName :: Name 
functionName = Name "filter"

isOddName :: Name 
isOddName = Name "isOdd"

bool :: Ty 
bool = TyCon (Name "Bool") []

filterLam1 :: Exp Id 
filterLam1 
  = Lam [xparam] [(StmtExp (Call Nothing isOddName [Var xid]))]

filterLam2 :: Exp Id 
filterLam2 
  = Lam [xparam] [StmtExp (Call Nothing (Name "isLt") [Var xid, Var yid])]

lamDefMap :: Map Name [LamDef] 
lamDefMap   
  = Map.insert functionName [case1, case2]
                            Map.empty 

case1 :: LamDef 
case1 = LamDef [xparam]
               [StmtExp (Call Nothing isOddName [Var xid])]

case2 :: LamDef 
case2 = LamDef [xparam]
               [StmtExp (Call Nothing (Name "isLt") [ Var xid
                                                    , Var yid])]

xparam :: Param Id 
xparam = Typed xid word 

xid :: Id 
xid = Id (Name "x") word 

yid :: Id 
yid = Id (Name "y") word

-- results 

cs :: String 
cs = "TestContract_filter"

tc :: Ty 
tc = TyCon (Name cs) []

dcn0 :: Name 
dcn0 = Name (cs ++ "0")

dcts0 :: [Ty]
dcts0 = []

dc0 :: Constr 
dc0 = Constr dcn0 dcts0

dcn1 :: Name 
dcn1 = Name (cs ++ "1")

dcts1 :: [Ty]
dcts1 = [word]

dc1 :: Constr 
dc1 = Constr dcn1 dcts1 

dt :: DataTy 
dt = DataTy (Name cs) [] [dc0, dc1]
