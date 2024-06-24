{
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Solcore.Frontend.Lexer.SolcoreLexer where

import Control.Monad
}


%wrapper "monadUserState"

$digit = 0-9            -- digits
$lower = [a-z \_]       -- lower case chars 
$upper = [A-Z]          -- upper case chars
$alpha = [a-zA-Z]       -- alphabetic characters

-- second RE macros

@identifier = $lower[$alpha $digit]* -- identifiers
@tycon      = $upper[$alpha $digit]* -- type constructor
@number     = $digit+


-- tokens declarations

tokens :-
        -- whitespace and comments 
        
        <0>    $white+                           ;
        <0>    "//" .*                           ;
        <0>   "/*"                               {nestComment `andBegin` state_comment}
        <0>   "*/"                               {\ _ _ -> alexError "Error: unexpected close comment!"}
        <state_comment> "/*"                     {nestComment}
        <state_comment> "*/"                     {unnestComment}
        <state_comment> .                        ;  
        <state_comment> \n                       ;

        -- keywords, and operators 
        
        <0>    "contract"                        {simpleToken TContract}
        <0>    "import"                          {simpleToken TImport}
        <0>    "let"                             {simpleToken TLet}
        <0>    "data"                            {simpleToken TData}
        <0>    "type"                            {simpleToken TType}
        <0>    "."                               {simpleToken TDot}
        <0>    ":"                               {simpleToken TColon}
        <0>    "="                               {simpleToken TEq}
        <0>    "class"                           {simpleToken TClass}
        <0>    "instance"                        {simpleToken TInstance}
        <0>    "if"                              {simpleToken TIf}
        <0>    "match"                           {simpleToken TMatch}
        <0>    "while"                           {simpleToken TWhile}
        <0>    "function"                        {simpleToken TFunction}
        <0>    "constructor"                     {simpleToken TConstructor}
        <0>    "return"                          {simpleToken TReturn}
        <0>    "lam"                             {simpleToken TLam}
        <0>    "->"                              {simpleToken TArrow}
        <0>    "=>"                              {simpleToken TDArrow}
        <0>    ";"                               {simpleToken TSemi}
        <0>    "_"                               {simpleToken TWildCard}
        <0>    "("                               {simpleToken TLParen}
        <0>    ")"                               {simpleToken TRParen}
        <0>    "{"                               {simpleToken TLBrace}
        <0>    "}"                               {simpleToken TRBrace}
        <0>    ","                               {simpleToken TComma}
        <0>    "["                               {simpleToken TLBrack}
        <0>    "]"                               {simpleToken TRBrack}
        <0>    "|"                               {simpleToken TBar}
        <0>    @tycon                            {mkCon}
        <0>    @identifier                       {mkIdent}
        <0>    @number                           {mkNumber}

        -- string literals 

        <0> \"                                   {enterString `andBegin` state_string}
        <state_string> \\n                       {emit '\n'}
        <state_string> \\t                       {emit '\t'}
        <state_string>  \\\"                     {emit '\"'}
        <state_string>  \"                       {exitString `andBegin` 0}
        <state_string>  .                        {emitCurrent}

{
-- user state 

data AlexUserState 
  = AlexUserState {
      nestLevel :: Int 
    , strStart :: AlexPosn
    , strBuffer :: String 
    }

alexInitUserState :: AlexUserState 
alexInitUserState 
  = AlexUserState 0 (AlexPn 0 0 0) []

get :: Alex AlexUserState
get = Alex $ \s -> Right (s, alex_ust s)

put :: AlexUserState -> Alex ()
put s' = Alex $ \s -> Right (s{alex_ust = s'}, ())

modify :: (AlexUserState -> AlexUserState) -> Alex ()
modify f 
  = Alex $ \s -> Right (s{alex_ust = f (alex_ust s)}, ())

alexEOF :: Alex Token
alexEOF = do
  (pos, _, _, _) <- alexGetInput
  startCode <- alexGetStartCode
  when (startCode == state_comment) $
    alexError "Error: unclosed comment"
  when (startCode == state_string) $
    alexError "Error: unclosed string"
  pure $ Token (position pos) TEOF

-- FIXME: Use AlexPosn in the token type to represent the location.

position :: AlexPosn -> (Int, Int)
position (AlexPn _ x y) = (x,y)

-- token definition

data Token
  = Token {
      pos :: (Int, Int)
    , lexeme :: Lexeme 
    } deriving (Eq, Ord, Show)

data Lexeme    
  = TIdent { unIdent :: String }
  | TTycon { unCon :: String }
  | TNumber { unNum :: Int }
  | TString { unStr :: String }
  | TContract 
  | TImport 
  | TLet
  | TEq 
  | TDot
  | TColon
  | TComma
  | TClass 
  | TInstance 
  | TData 
  | TType
  | TMatch
  | TIf 
  | TWhile
  | TFunction
  | TConstructor
  | TReturn 
  | TLam 
  | TSemi
  | TWildCard
  | TArrow
  | TDArrow
  | TLParen 
  | TRParen
  | TLBrace
  | TRBrace
  | TLBrack
  | TRBrack
  | TBar 
  | TEOF 
  deriving (Eq, Ord, Show)

-- Functions to create tokens 

mkIdent :: AlexAction Token 
mkIdent (st, _, _, str) len 
  = case take len str of 
      "match" -> return $ Token (position st) TMatch
      "data" -> return $ Token (position st) TData 
      "type" -> return $ Token (position st) TType
      "import" -> return $ Token (position st) TImport 
      "contract" -> return $ Token (position st) TContract
      "function" -> return $ Token (position st) TFunction
      "constructor" -> return $ Token (position st) TConstructor
      "return" -> return $ Token (position st) TReturn 
      "let" -> return $ Token (position st) TLet
      "lam" -> return $ Token (position st) TLam
      _ -> return $ Token (position st) (TIdent $ take len str)

mkCon :: AlexAction Token 
mkCon (st, _, _, str) len 
  = pure $ Token (position st) (TTycon (take len str))

mkNumber :: AlexAction Token
mkNumber (st, _, _, str) len 
  = pure $ Token (position st) (TNumber $ read $ take len str)

simpleToken :: Lexeme -> AlexAction Token 
simpleToken lx (st, _, _, _) len 
  = return $ Token (position st) lx

-- string literals 

enterString :: AlexAction Token
enterString inp@(pos, _, _, _) len 
  = do
      modify $ \s -> s{ strStart = pos
                      , strBuffer = '"' : strBuffer s
                      }
      skip inp len

exitString :: AlexAction Token 
exitString inp@(pos, _, _, _) len 
  = do
      s <- get
      put s{strStart = AlexPn 0 0 0, strBuffer = []}
      let tk = TString $ reverse $ '"' : strBuffer s
      return $ Token (position pos) tk

emit :: Char -> AlexAction Token
emit c inp@(_, _, _, str) len = do
  modify $ \s -> s{strBuffer = c : strBuffer s}
  skip inp len

emitCurrent :: AlexAction Token
emitCurrent (_, _, _, []) _ = alexError "Error: Expecting EOF!"
emitCurrent inp@(_, _, _, (c : _)) len = do
  modify $ \s -> s{strBuffer = c : strBuffer s}
  skip inp len

-- dealing with comments

nestComment :: AlexAction Token
nestComment input len = do
  modify $ \s -> s{nestLevel = nestLevel s + 1}
  skip input len

unnestComment :: AlexAction Token 
unnestComment input len 
  = do
      s <- get
      let level = (nestLevel s) - 1
      put s{nestLevel = level}
      when (level == 0) $
        alexSetStartCode 0
      skip input len


lexer :: String -> Either String [Token]
lexer s = runAlex s go 
  where 
    go = do 
      output <- alexMonadScan 
      if lexeme output == TEOF then 
        pure [output]
      else (output :) <$> go
}
