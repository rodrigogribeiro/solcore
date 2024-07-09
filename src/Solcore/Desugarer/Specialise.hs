module Solcore.Desugarer.Specialise where
import Control.Monad

import Solcore.Frontend.Syntax
import Solcore.Frontend.TypeInference.Id


specialiseCompUnit :: CompUnit Id -> IO (CompUnit Id)
specialiseCompUnit (CompUnit imports contracts) = do
    contracts' <- forM contracts specialiseContract

    return $ CompUnit imports contracts'
specialiseContract :: Contract Id -> IO (Contract Id)
specialiseContract (Contract name args decls) = do
    forM_ decls processDecl 
    return $ Contract name args decls
    where
      processDecl(FunDecl (FunDef sig body)) = do
        let name = show $ sigName sig
        let params = sigParams sig
        let result = sigReturn sig
        let s = unwords[name, ":", show params, show result]
        putStrLn s
        print body
      processDecl _ = pure ()