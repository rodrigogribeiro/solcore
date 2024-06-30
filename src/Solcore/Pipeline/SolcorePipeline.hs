module Solcore.Pipeline.SolcorePipeline where

import Control.Monad

import Options.Applicative 

import Solcore.Desugarer.MatchCompiler 
import Solcore.Frontend.Lexer.SolcoreLexer
import Solcore.Frontend.Parser.SolcoreParser
import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.TypeInference.TcContract
import Solcore.Frontend.TypeInference.TcEnv

-- main compiler driver function 

pipeline :: IO () 
pipeline = do 
  opts <- argumentsParser
  content <- readFile (fileName opts)
  case runAlex content parser of 
    Left err -> putStrLn err 
    Right ast ->
      case typeInfer ast of
        Left err -> putStrLn err 
        Right env -> 
          do 
            when (enableLog env) (mapM_ putStrLn (reverse $ logs env))
            res <- matchCompiler ast 
            case res of 
              Right r -> putStrLn $ pretty r 
              Left err -> putStrLn err 

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
