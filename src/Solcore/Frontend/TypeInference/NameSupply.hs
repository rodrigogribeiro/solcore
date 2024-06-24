module Solcore.Frontend.TypeInference.NameSupply where 

addNumbers :: [String] -> [Integer] -> [String]
addNumbers names numbers = do 
  number <- numbers 
  name <- names 
  return $ name ++ show number

namePool :: [String]
namePool = names ++ addNumbers names [1 ..] 
  where 
    names = ["a", "b", "c", "d", "t", "u"]

type NS = [String]

deplete :: NS -> (String, NS)
deplete (x : xs) = (x, xs)

split :: NS -> (NS, NS)
split (x : y : zs) = (x : xs, y : ys) 
  where 
    (xs,ys) = split zs 
