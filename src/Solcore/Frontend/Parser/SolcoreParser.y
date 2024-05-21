{
module Solcore.Frontend.Parser.SolcoreParser (solCoreParser) where

import Solcore.Frontend.Lexer.SolcoreLexer
import Solcore.Frontend.Syntax.Contract
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.Stmt
import Solcore.Frontend.Syntax.Ty
}


%name parser
%monad {P}{(>>=)}{return}
%tokentype { Token }
%error     { parseError }


%token
      identifier {Token _ (TIdent $$)}
      number     {Token _ (TNumber $$)}
      tycon      {Token _ (TTycon $$)}
      'contract' {Token _ TContract}
      'import'   {Token _ TImport}
      '='        {Token _ TEq}
      '.'        {Token _ TDot}
      'class'    {Token _ TClass}
      'instance' {Token _ TInstance}
      'data'     {Token _ TData}
      'type'     {Token _ TType}
      'switch'   {Token _ TSwitch}
      'case'     {Token _ TCase}
      'if'       {Token _ TIf}
      'while'    {Token _ TWhile}
      'function' {Token _ TFunction}
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
Contract : 'contract' Name '{' DeclList '}'        { Contract $2 $4 }

DeclList :: { [Decl] }
DeclList : DeclList Decl                           { $2 : $1 }
         | {- empty -}                             { [] }

-- declarations 

Decl :: { Decl }
Decl : FieldDef                                    {FieldDecl $1}
     | DataDef                                     {DataDecl $1}
     | SymDef                                      {SymDecl $1}
     | ClassDef                                    {ClassDecl $1}
     | InstDef                                     {InstDecl $1}
     | Function                                    {FunDecl $1}

-- fields 

FieldDef :: { Field }
FieldDef : Type Name                               {Field $2 $1 Nothing}

-- algebraic data types 

DataDef :: { DataTy }
DataDef : 'data' Name OptParam '=' Constrs         {DataTy $2 $3 $5}     

Constrs :: {[Constr]}
Constrs : Constr ConstrList                        {$1 : $2}   

ConstrList :: { [Constr] }
ConstrList : '|' Constr ConstrList                 {$2 : $3}
           | {- empty -}                           { [] }  

Constr :: { Constr }
Constr : Con '(' OptTypeParam ')'                  { Constr $1 $3 }

-- type synonyms 

SymDef :: { TySym }
SymDef : 'type' Con OptParam '=' Type               {TySym $2 $3 $5}

-- class definitions 

ClassDef :: { Class }
ClassDef 
  : 'class' Context '=>' Var ':' Con OptParam '{' Signatures '}' {Class $2 $6 $7 $4 $9}

OptParam :: { [Tyvar] }
OptParam :  '[' Var VarCommaList ']'               {$2 : $3}
         | {- empty -}                             {[]}

VarCommaList :: { [Tyvar] }
VarCommaList : ',' Var VarCommaList                {$2 : $3}          
             | {- empty -}                         {[]}

Context :: {[Pred]}
Context : Constraint                               { [ $1 ] }
        | {- empty -}                              { [] }
        | '(' Constraint ConstraintList ')'        { $2 : $3 }   

ConstraintList :: { [Pred] }
ConstraintList : ',' Constraint ConstraintList     {$2 : $3}
               | {- empty -}                       {[]}

Constraint :: { Pred }
Constraint : Type ':' Con OptTypeParam             {Pred $3 $1 $4} 

Signatures :: { [Signature] }
Signatures : Signature ';' Signatures              {$1 : $3}
           | {- empty -}                           {[]}

Signature :: { Signature }
Signature : Name '(' ParamList ')' ':' Type        {Signature $1 $3 $6}

ParamList :: { [(Name, Ty)] }
ParamList : {- empty -}                            {[]}
          | Param  ParamCommaList                  {$1 : $2}

ParamCommaList :: { [(Name, Ty)] }
ParamCommaList : ',' Param ParamCommaList          {$2 : $3}
               | {- empty -}                       {[]}

Param :: { (Name, Ty) }
Param : Name ':' Type                              {($1, $3)}

-- instance declarations 

InstDef :: { Instance }
InstDef : 'instance' Context '=>' Type ':' Con OptTypeParam '{' Functions '}' { Instance $2 $6 $7 $4 $9 }

OptTypeParam :: { [Ty] }
OptTypeParam : '[' Type TypeCommaList ']'          {$2 : $3}
             | {- empty -}                         {[]}

TypeCommaList :: { [Ty] }
TypeCommaList : ',' Type TypeCommaList             {$2 : $3}
              | {- empty -}                        {[]}

Functions :: { [FunDef] }
Functions : Function Functions                     {$1 : $2}
          | {- empty -}                            {[]}

-- Function declaration 

Function :: { FunDef }
Function : 'function' Name '(' ParamList ')' '->' Type Body {FunDef $2 $7 $4 $8}

-- Function body 

Body :: { [Stmt] }
Body : '{' StmtList '}'                            {$2} 

StmtList :: { [Stmt] }
StmtList : Stmt ';' StmtList                       {$1 : $3}
         | {- empty -}                             {[]}

-- Statements 


Stmt :: { Stmt }
Stmt : Name '=' Expr                               {$1 := $3}
     | 'if' Expr Body                              {If $2 $3}
     | 'while' Expr Body                           {While $2 $3}
     | Expr                                        {StmtExp $1}

-- Expressions 

Expr :: { Exp }
Expr : Name                                        {Var $1}
     | Con '(' ExprList ')'                        {Con $1 $3}
     | Literal                                     {Lit $1}
     | '(' Expr ')'                                {$2}
     | Name '(' ExprList ')'                       {Call $1 $3}
     | 'switch' Expr '{' Equations  '}'            {Switch $2 $4}

ExprList :: { [Exp] }
ExprList : {- empty -}                             {[]}
         | Expr ExprCommaList                      {$1 : $2}

ExprCommaList :: { [Exp] }
ExprCommaList : ',' Expr ExprCommaList             {$2 : $3}
              | {- empty -}                        {[]}

-- Pattern matching equations 

Equations :: { [(Pat, [Stmt])]}
Equations : Equation Equations                     {$1 : $2}
          | {- empty -}                            {[]}

Equation :: { (Pat, [Stmt]) }
Equation : 'case' Pattern ':' StmtList             {($2, $4)}

Pattern :: { Pat }
Pattern : Name                                     {PVar $1}
        | Con '(' PatList ')'                      {PCon $1 $3}
        | '_'                                      {PWildcard}
        | Literal                                  {PLit $1}

PatList :: { [Pat] }
PatList : {- empty -}                              {[]}
        | Pattern PatternCommaList                 {$1 : $2}

PatternCommaList :: { [Pat] }
PatternCommaList : ',' Pattern PatternCommaList    {$2 : $3}
                 | {- empty -}                     {[]}

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
QualName : Name                                    { [$1] }
         | QualName '.' Name                       { $3 : $1 }

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
