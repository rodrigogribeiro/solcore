# Solcore

1. Grammar specification
   - Terminals are specified using quotes as 'this'. Other terminals are 
        - numbers
        - identifiers 
   - epsilon denotes the empty string.

```
CompilationUnit -> ImportList ContractList
ImportList -> ImportList Import | epsilon 
Import -> 'import' QualName ';'
ContractList -> ContractList Contract | epsilon 
Contract -> 'contract' Name '{' DeclList '}'
DeclList -> DeclList Decl | epsilon 
Decl -> FieldDef | DataDef | SymDef | ClassDef | InstDef | Function
FieldDef -> Type Name                           
DataDef -> 'data' Name '=' Constrs
Constrs -> Constr ConstrList
ConstrList -> '|' Constr ConstrList | epsilon 
Constr -> Con '(' OptTypeParam ')'
SymDef -> 'type' Con OptParam '=' Type
ClassDef -> 'class' Context '=>' Var ':' Con OptParam '{' Signatures '}'
OptParam -> '[' Var VarCommaList ']' | epsilon 
VarCommaList -> ',' Var VarCommaList | epsilon
Context -> Constraint | '(' Constraint ConstraintList ')' | epsilon
ConstraintList -> ',' Constraint ConstraintList | epsilon 
Constraint -> Type ':' Con OptTypeParam
Signatures -> Signature ';' Signatures  | epsilon 
Signature -> Name '(' ParamList ')' ':' Type
ParamList -> Param ParamCommaList | epsilon 
ParamCommaList -> ',' Param ParamCommaList  | epsilon 
Param -> Name ':' Type  
InstDef -> 'instance' Context '=>' Type ':' Con '[' OptTypeParam ']' '{' Functions '}'
OptTypeParam -> '[' Type TypeCommaList ']' | epsilon 
TypeCommaList -> ',' Type TypeCommaList  | epsilon 
Functions -> Function Functions  | epsilon
Function -> 'function' Name '(' ParamList ')' '->' Type Body
Body -> '{' StmtList '}'
StmtList -> Stmt ';' StmtList | epsilon 
Stmt -> Name '=' Expr | 'if' Expr Body | 'while' Expr Body | Expr
Expr -> Name | Con '(' ExprList ')' | Literal | '(' Expr ')' | Name '(' ExprList ')' 
     | 'switch' Expr '{' Equations '}'
ExprList -> Expr ExprCommaList  | epsilon
ExprCommaList -> ',' Expr ExprCommaList | epsilon 
Equations -> Equation Equations | epsilon 
Equation -> 'case' Pattern ':' StmtList
Pattern -> Name | Con '(' PatList ')' | '_' | Literal 
PatList -> Pattern PatternCommaList | epsilon 
PatternCommaList -> ',' Pattern PatternCommaList | epsilon 
Literal -> number
Type -> Con OptTypeParam | Var 
Var -> Name 
Con -> tycon
QualName -> Name | QualName '.' Name
Name -> identifier
```

