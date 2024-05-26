{
module Solcore.Frontend.Parser.SolcoreParser where

import Solcore.Frontend.Lexer.SolcoreLexer
import Solcore.Frontend.Syntax.Contract
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.Stmt
import Solcore.Frontend.Syntax.Ty
}


%name parser CompilationUnit
%monad {P}{(>>=)}{return}
%tokentype { Token }
%error     { parseError }


%token
      identifier {Token _ (TIdent $$)}
      number     {Token _ (TNumber $$)}
      tycon      {Token _ (TTycon $$)}
      'contract' {Token _ TContract}
      'import'   {Token _ TImport}
      'let'      {Token _ TLet}
      '='        {Token _ TEq}
      '.'        {Token _ TDot}
      'class'    {Token _ TClass}
      'instance' {Token _ TInstance}
      'data'     {Token _ TData}
      'type'     {Token _ TType}
      'switch'   {Token _ TSwitch}
      'case'     {Token _ TCase}
--      'if'       {Token _ TIf}
--      'while'    {Token _ TWhile}
      'function' {Token _ TFunction}
      'constructor' {Token _ TConstructor}
      ';'        {Token _ TSemi}
      ':'        {Token _ TColon}
      ','        {Token _ TComma}
      '->'       {Token _ TArrow}
      '_'        {Token _ TWildCard}
      '=>'       {Token _ TDArrow}
      '('        {Token _ TLParen}
      ')'        {Token _ TRParen}
      '{'        {Token _ TLBrace}
      '}'        {Token _ TRBrace}
      '['        {Token _ TLBrack}
      ']'        {Token _ TRBrack}
      '|'        {Token _ TBar}


%%
-- compilation unit definition 

CompilationUnit :: {CompUnit}
CompilationUnit : ImportList ContractList          { CompUnit $1 $2 } 

ImportList :: { [Import] }
ImportList : ImportList Import                     { $2 : $1 }
           | {- empty -}                           { [] }

Import :: { Import }
Import : 'import' QualName ';'                     { Import (QualName $2) }

ContractList :: {[Contract]}
ContractList : ContractList Contract               { $2 : $1 }
             | {- empty -}                         { [] }

-- contracts 

Contract :: { Contract }
Contract : 'contract' Con OptParam '{' DeclList '}' { Contract $2 $3 $5 }

DeclList :: { [Decl] }
DeclList : Decl DeclList                           { $1 : $2 }
         | {- empty -}                             { [] }

-- declarations 

Decl :: { Decl }
Decl : FieldDef                                    {FieldDecl $1}
     | DataDef                                     {DataDecl $1}
     | SymDef                                      {SymDecl $1}
     | ClassDef                                    {ClassDecl $1}
     | InstDef                                     {InstDecl $1}
     | Function                                    {FunDecl $1}
     | Constructor                                 {ConstrDecl $1}

-- fields 

FieldDef :: { Field }
FieldDef : Name ':' Type                           {Field $1 $3 Nothing}

-- algebraic data types 

DataDef :: { DataTy }
DataDef : 'data' Con OptParam '=' Constrs          {DataTy $2 $3 $5}     

Constrs :: {[Constr]}
Constrs : Constr '|' Constrs                       {$1 : $3}
        | Constr                                   {[$1]}

Constr :: { Constr }
Constr : Con OptTypeParam                          { Constr $1 $2 }

-- type synonyms 

SymDef :: { TySym }
SymDef : 'type' Con OptParam '=' Type               {TySym $2 $3 $5}

-- class definitions 

ClassDef :: { Class }
ClassDef 
  : 'class' ContextOpt Var ':' Con OptParam ClassBody {Class $2 $5 $6 $3 $7}

ClassBody :: {[Signature]}
ClassBody : '{' Signatures '}'                     {$2}

OptParam :: { [Tyvar] }
OptParam :  '[' VarCommaList ']'                   {$2}
         | {- empty -}                             {[]}

VarCommaList :: { [Tyvar] }
VarCommaList : Var ',' VarCommaList                {$1 : $3} 
             | Var                                 {[$1]}

ContextOpt :: {[Pred]}
ContextOpt : {- empty -}                           {[]}
           | Context                               {$1}

Context :: {[Pred]}
Context : '[' ConstraintList ']' '=>'              { $2 }   

ConstraintList :: { [Pred] }
ConstraintList : Constraint ',' ConstraintList     {$1 : $3}
               | Constraint                        {[$1]}

Constraint :: { Pred }
Constraint : Type ':' Con OptTypeParam             {Pred $3 $1 $4} 

Signatures :: { [Signature] }
Signatures : Signature ';' Signatures              {$1 : $3}
           | {- empty -}                           {[]}

Signature :: { Signature }
Signature : Name '(' ParamList ')' '->' Type        {Signature $1 $3 $6}

ParamList :: { [(Name, Ty)] }
ParamList : Param                                  {[$1]}
          | Param  ',' ParamList                   {$1 : $3}
          | {- empty -}                            {[]}

Param :: { (Name, Ty) }
Param : Name ':' Type                              {($1, $3)}

-- instance declarations 

InstDef :: { Instance }
InstDef : 'instance' ContextOpt Type ':' Con OptTypeParam InstBody { Instance $2 $5 $6 $3 $7 }

OptTypeParam :: { [Ty] }
OptTypeParam : '[' TypeCommaList ']'          {$2}
             | {- empty -}                    {[]}

TypeCommaList :: { [Ty] }
TypeCommaList : Type ',' TypeCommaList             {$1 : $3}
              | Type                               {[$1]}

Functions :: { [FunDef] }
Functions : Function Functions                     {$1 : $2}
          | {- empty -}                            {[]}

InstBody :: {[FunDef]}
InstBody : '{' Functions '}'                       {$2}

-- Function declaration 

Function :: { FunDef }
Function : 'function' Name '(' ParamList ')' OptRetTy Body {FunDef $2 $6 $4 $7}

OptRetTy :: { Maybe Ty }
OptRetTy : '->' Type                               {Just $2}
         | {- empty -}                             {Nothing}

-- Contract constructor 

Constructor :: { Constructor }
Constructor : 'constructor' '(' ParamList ')' Body {Constructor $3 $5}

-- Function body 

Body :: { [Stmt] }
Body : '{' StmtList '}'                            {$2} 

StmtList :: { [Stmt] }
StmtList : Stmt ';' StmtList                       {$1 : $3}
         | {- empty -}                             {[]}

-- Statements 


Stmt :: { Stmt }
Stmt : Name '=' Expr                               {$1 := $3}
     | 'let' Name ':' Type InitOpt                 {Let $2 $4 $5}
--     | 'if' Expr Body                              {If $2 $3}
--     | 'while' Expr Body                           {While $2 $3}
     | Expr                                        {StmtExp $1}

InitOpt :: {Maybe Exp}
InitOpt : {- empty -}                              {Nothing}
        | '=' Expr                                 {Just $2}

-- Expressions 

Expr :: { Exp }
Expr : Name                                        {Var $1}
     | Con ConArgs                                 {Con $1 $2}
     | Literal                                     {Lit $1}
     | '(' Expr ')'                                {$2}
     | Name FunArgs                                {Call $1 $2}
     | 'switch' Expr '{' Equations  '}'            {Switch $2 $4}

ConArgs :: {[Exp]}
ConArgs : '[' ExprCommaList ']'                    {$2}
        | {- empty -}                              {[]} 

FunArgs :: {[Exp]} 
FunArgs : '(' ExprCommaList ')'                    {$2}

ExprCommaList :: { [Exp] }
ExprCommaList : Expr                               {[$1]}
              | {- empty -}                         {[]}
              | Expr ',' ExprCommaList             {$1 : $3}

-- Pattern matching equations 

Equations :: { [(Pat, [Stmt])]}
Equations : Equation Equations                     {$1 : $2}
          | {- empty -}                            {[]}

Equation :: { (Pat, [Stmt]) }
Equation : 'case' Pattern ':' StmtList             {($2, $4)}

Pattern :: { Pat }
Pattern : Name                                     {PVar $1}
        | Con PatternList                          {PCon $1 $2}
        | '_'                                      {PWildcard}
        | Literal                                  {PLit $1}
        | '(' Pattern ')'                          {$2}

PatternList :: {[Pat]}
PatternList : '[' PatList ']'                      {$2}
            | {- empty -}                          {[]}

PatList :: { [Pat] }
PatList : Pattern                                  {[$1]}
        | Pattern ',' PatList                      {$1 : $3}

-- literals 

Literal :: { Literal }
Literal : number                                   {IntLit $ toInteger $1}

-- basic type definitions 


Type :: { Ty }
Type : Con OptTypeParam                            {TyCon $1 $2}
     | Var                                         {TyVar  $1}
        
Var :: { Tyvar }
Var : Name                                         {TVar $1}  

Con :: { Name }
Con : tycon                                        { Name $1 }

QualName :: { [Name] }  
QualName : Con                                     { [$1] }
         | QualName '.' Con                        { $3 : $1 }

Name :: { Name }
Name : identifier                                  { Name $1 }

{
type P = Either String

parseError :: [Token] -> P a
parseError ts 
  = Left $ "syntax error at " <> (pErr ts)
  where  
    loc (x,y) = unwords ["line:", show x, "column:", show y]
    pErr (t : _) = loc (pos t)
    pErr _ = "EOF"


solCoreParser :: String -> Either String CompUnit
solCoreParser = parser . lexer
}
