module Solcore.Primitives.Primitives where 

import Solcore.Frontend.Syntax.Contract 
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.Stmt 
import Solcore.Frontend.Syntax.Ty 

-- basic types 

word :: Ty 
word = TyCon "Word" []

primAddWord :: (Name, Scheme)
primAddWord = ("primAddWord", monotype (word :-> word :-> word))

primEqWord :: (Name, Scheme)
primEqWord = ("primEqWord", monotype (word :-> word :-> word))

string :: Ty 
string = TyCon "String" []

stack :: Ty -> Ty 
stack t = TyCon "Stack" [t]

unit :: Ty 
unit = TyCon "Unit" []

arr :: Name  
arr = "->"


