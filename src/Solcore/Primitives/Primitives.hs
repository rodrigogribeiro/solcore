module Solcore.Primitives.Primitives where 

import Solcore.Frontend.Syntax.Contract 
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.Stmt 
import Solcore.Frontend.Syntax.Ty 
-- 

-- desugaring interface for load / store 

load :: Exp -> Exp 
load e = Call Nothing loadName [e] 

store :: Exp -> Exp -> Exp 
store e1 e2 = undefined

-- basic names 

storeName :: Name 
storeName = Name "store"

loadName :: Name 
loadName = Name "load"


