{
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Solcore.Frontend.Lexer.SolcoreLexer (Token (..), Lexeme (..), lexer,position) where
}


%wrapper "posn"

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
      $white+       ;
      "//" .*       ;
      @number       {mkNumber}
      "contract"    {simpleToken TContract}
      "import"      {simpleToken TImport}
      "let"         {simpleToken TLet}
      "data"        {simpleToken TData}
      "type"        {simpleToken TType}
      "."           {simpleToken TDot}
      ":"           {simpleToken TColon}
      "="           {simpleToken TEq}
      "class"       {simpleToken TClass}
      "instance"    {simpleToken TInstance}
      "if"          {simpleToken TIf}
      "switch"      {simpleToken TSwitch}
      "case"        {simpleToken TCase}
      "while"       {simpleToken TWhile}
      "function"    {simpleToken TFunction}
      "constructor" {simpleToken TConstructor}
      "->"          {simpleToken TArrow}
      "=>"          {simpleToken TDArrow}
      ";"           {simpleToken TSemi}
      "_"           {simpleToken TWildCard}
      "("           {simpleToken TLParen}
      ")"           {simpleToken TRParen}
      "{"           {simpleToken TLBrace}
      "}"           {simpleToken TRBrace}
      ","           {simpleToken TComma}
      "["           {simpleToken TLBrack}
      "]"           {simpleToken TRBrack}
      "|"           {simpleToken TBar}
      @tycon        {mkCon}
      @identifier   {mkIdent}

{
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
  | TSwitch
  | TCase
  | TIf 
  | TWhile
  | TFunction
  | TConstructor 
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
  deriving (Eq, Ord, Show)

position :: AlexPosn -> (Int, Int)
position (AlexPn _ x y) = (x,y)

mkIdent :: AlexPosn -> String -> Token 
mkIdent p s
  | s == "if"   = Token (position p) TIf
  | s == "while" = Token (position p) TWhile
  | s == "switch" = Token (position p) TSwitch 
  | s == "case" = Token (position p) TCase
  | s == "data" = Token (position p) TData 
  | s == "type" = Token (position p) TType
  | s == "import" = Token (position p) TImport 
  | s == "contract" = Token (position p) TContract
  | s == "function" = Token (position p) TFunction
  | s == "constructor" = Token (position p) TConstructor
  | s == "let" = Token (position p) TLet
  | otherwise = Token (position p) (TIdent s)

mkCon :: AlexPosn -> String -> Token 
mkCon p s = Token (position p) (TTycon s)

mkNumber :: AlexPosn -> String -> Token
mkNumber p s = Token (position p) (TNumber $ read s)

simpleToken :: Lexeme -> AlexPosn -> String -> Token
simpleToken lx p _ = Token (position p) lx

lexer :: String -> [Token]
lexer = alexScanTokens
}
