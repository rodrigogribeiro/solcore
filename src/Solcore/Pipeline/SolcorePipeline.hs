module Solcore.Pipeline.SolcorePipeline where

import qualified Data.Map as Map

import Options.Applicative 

import Solcore.Desugarer.MatchCompiler
import Solcore.Desugarer.InternalSyntaxCompiler
import Solcore.Frontend.Lexer.SolcoreLexer
import Solcore.Frontend.Parser.SolcoreParser
import Solcore.Frontend.Pretty.SolcorePretty 
import Solcore.Frontend.Syntax.Contract
import Solcore.Frontend.TypeInference.Checker
import Solcore.Frontend.TypeInference.ISyntax
import Solcore.Frontend.TypeInference.Specialise
import Solcore.Frontend.TypeInference.TcMonad hiding (info)

-- main compiler driver function 

pipeline :: IO () 
pipeline = do 
  opts <- argumentsParser
  content <- readFile (fileName opts)
  case runAlex content parser of 
    Left err -> putStrLn err 
    Right ast -> runPipeline ast 


runPipeline :: CompUnit -> IO ()
runPipeline ast = 
  do 
    res <- matchCompiler ast 
    case res of 
      Right r -> mapM_ processProg (internalCompiler r)
      Left err -> putStrLn err 

-- type checking 

processProg :: Prog -> IO () 
processProg prog 
  = do 
      let (res, state) = runTCM (processProg' prog)
      case res of
        Left err -> putStrLn err 
        Right t -> print prog 

processProg' prog@(Prog decls) 
  = do 
      tiProg prog 
      tld <- buildTLD decls 
      case Map.lookup entrypoint tld of
        Nothing -> return ()
        Just def -> withLogging $ specialiseEntry entrypoint
  where
    entrypoint = "main"     

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
