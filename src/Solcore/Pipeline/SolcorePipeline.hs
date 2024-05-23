module Solcore.Pipeline.SolcorePipeline where

import Options.Applicative 
import Solcore.Frontend.Parser.SolcoreParser
import Solcore.Frontend.Pretty.SolcorePretty 

-- main compiler driver function 

pipeline :: IO () 
pipeline = do 
  opts <- argumentsParser
  content <- readFile (fileName opts)
  case solCoreParser content of 
    Left err -> putStrLn err 
    Right ast -> putStrLn $ pretty ast


-- parsing command line arguments 

data Option 
  = Option {
      fileName :: FilePath
    } deriving (Eq, Show)

options :: Parser Option 
options 
  = Option <$> strOption (
                  long "file"
               <> short 'f'
               <> metavar "FILE"
               <> help "Input file name")

argumentsParser :: IO Option 
argumentsParser = do 
  let opts = info (options <**> helper)
                  (fullDesc <> 
                   header "Solcore - solidity core language")
  opt <- execParser opts
  return opt 
