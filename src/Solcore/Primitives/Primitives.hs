module Solcore.Primitives.Primitives where 

import Solcore.Frontend.Syntax.Contract 
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.Stmt 
import Solcore.Frontend.Syntax.Ty 


primVals :: [(Name, Scheme)]
primVals =
  [ ("undefined", forAll "a" a)
  , ("zero", monotype $ int :-> int :-> int)
  , ("add", monotype $ int :-> int :-> int)
  , ("sub", monotype $ int :-> int :-> int)
  , ("even", monotype $ int :-> bool)
  , ("ifzero", forAll "a" $ int :-> a :-> a :-> a )
  , ("recInt", forAll "a" $ (int :-> a) :-> a :-> int :-> a)
  , ("true" , monotype bool)
  , ("false", monotype bool)
  , ("not", monotype $ bool :-> bool)
  , ("or", bool2)
  , ("and", bool2)
  , ("ifte", forAll "a" $ bool :-> a :-> a :-> a)
  , ("nil", forAll "a" $ list a)
  , ("cons", forAll "a" $ a :-> list a :-> list a)
  , ("foldr", forAll "a b" $ (a :-> b :-> b) :-> b :-> list a :-> b)
  , ("head", forAll "a" $ list a :-> a)
  , ("tail", forAll "a" $ list a :-> list a)
  , ("pair", forAll "a b" $ a :-> b :-> pair a b)
  , ("fst", forAll "a b" $ pair a b :-> a)
  , ("snd", forAll "a b" $ pair a b :-> b)
  , ("eq", Forall [va] $ [IsIn "Eq" a] :=> (a :-> a :-> bool))
  , ("newMRef", forAll "a" $ a :->  memo a)
  -- Constructors for primitive types
  , ("Unit", monotype unit)
  , ("True", monotype bool)
  , ("False", monotype bool)
  , ("Nil", forAll "a" $ list a)
  , ("Cons", forAll "a" $ a :-> list a :-> list a)
  -- methods for class Ref
  , ("load", Forall [va, vb] $ [InCls "Ref" a [b]] :=> (a :-> b))
  , ("store", Forall [va, vb] $ [InCls "Ref" a [b]] :=> (a :-> b :-> unit))
  ] where
  va = TVar (Name "a")
  vb = TVar (Name "b")
  a = TyVar va
  b = TyVar vb
  unit = TyCon (Name "Unit") []
  bool2 = monotype $ bool :-> bool :-> bool
  list x = TyCon "List" [x]
  stack x = TyCon "Stack" [x]
  memo x = TyCon "Memory" [x]
  pair x y = TyCon "Pair" [x, y]

primTypes :: [(Name, (Int, [Name]))]
primTypes =
  [ ("Int", (0, []))
  , ("Unit", (0, ["Unit"]))
  , ("Bool", (0, ["False", "True"]))
  , ("->",  (2, []))
  , ("List", (1, ["Nil", "Cons"]))
  ]

type ClassInfo = (Int, [Name])

primClasses =
  [ (Name "Eq", eqClassInfo)
  , (Name "Ref", refClassInfo)
  -- class a:IndexAccessible[indexType, memberType]
  -- , ("IndexAccessible", indexClassInfo)
  , (Name "MemoryBaseType", mbtClassInfo)
  ]

eqClassInfo :: ClassInfo
eqClassInfo = (0, [Name "eq"])

eqInstances :: [Inst]
eqInstances = [ [] :=> IsIn "Eq" int
               , [] :=> IsIn "Eq" bool
               , [IsIn cEq a] :=> IsIn cEq (list a)
              ] where
  a = TyVar (TVar $ Name "a")
  b = TyVar (TVar $ Name "b")
  list x = TyCon (Name "List") [x]
  cEq = "Eq"

refClassInfo :: ClassInfo
refClassInfo = (1, ["load"])

refInstances :: [Inst]
refInstances =
  [ [] :=> InCls "Ref" (stack int) [int]  -- inst Ref(int) (Stack Int)
    -- inst Ref a (Memory a)  becomes
  , [b :~:  a] :=> InCls (Name "Ref") (memo a)  [b] -- (b ~ a) => inst Ref b (Memory a)
  , [b :~: int] :=> InCls (Name "Ref") (TyCon (Name "SI") []) [b]
  ]
  where
    a = TyVar $ TVar (Name "a")
    b = TyVar $ TVar (Name "b")
    stack x = TyCon (Name "Stack") [x]
    memo x = TyCon (Name "Memory") [x]

indexClassInfo :: ClassInfo 
indexClassInfo = (2, ["indexAccess"])

-- class a:ReadFromMemory => a:MemoryBaseType
mbtClassInfo = (0, ["stride"])

int :: Ty 
int = TyCon (Name "Int") []

bool :: Ty 
bool = TyCon (Name "Bool") []
