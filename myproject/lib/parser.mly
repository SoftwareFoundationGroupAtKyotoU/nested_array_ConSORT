%{
open Syntax
%}

%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token PLUS MINUS STAR LT EQ
%token IFNP THEN ELSE NOT OR AND
%token LET IN ALIAS ASSERT ALLOC REF INT
%token COLON COMMA ASSIGN NONDET WAVE RARROW SEMI
%token EOF

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.exp> toplevel 
%%(*?*)

toplevel :
    LBRACE e=Expr RBRACE EOF { e }

Expr :
    e=IfnpExpr { e }
  | e=LetExpr { e }
  | e1=PreSEMIExpr SEMI e2=Expr { PreSEMIExpr(e1, e2) }
  | e=AExpr { e }

IfnpExpr :
    IFNP x=ID THEN t=Expr ELSE e=Expr { IfnpExp (x, t, e) }

LetExpr :
    LET x=ID EQ ALLOC y=ID COLON ty=SimpleTyExpr REF IN e=Expr { LetAllocExp(x, Var y, SRef ( ty ), e) }
  | LET x=ID EQ STAR y=ID IN e=Expr { LetDerefExp(x, Var y, e) }
  | LET x=ID EQ e1=BinOpExpr IN e2=Expr { LetBinOpExp(x, e1, e2) }
  | LET x=ID EQ e1=Expr IN e2=Expr { LetBindExp(x, e1, e2) }
  | LET x=ID EQ app=FunCallExpr IN e=Expr { LetFunCall(x, app, e) }

SimpleTyExpr :
    INT { SInt }
  | ty=SimpleTyExpr REF { SRef (ty) }

BinOpExpr :
  | x=AExpr PLUS y=AExpr { BinOp (Plus, x, y) }
  | x=AExpr MINUS y=AExpr { BinOp (Minus, x, y) }
  | x=AExpr STAR y=AExpr { BinOp (Mult, x, y) }

FunCallExpr :
  | i=ID LPAREN v=VarSeq RPAREN { FunCall(i, v)}

VarSeq :
    i=ID { [i] }
  | i=ID COMMA v=VarSeq { i :: v } 


PreSEMIExpr :
    x=ID ASSIGN y=ID { Assign(x, y) }
  | ALIAS LPAREN x=ID EQ y=ID PLUS z=ID RPAREN { AliasAddPtr(x, y, z) }
  | ALIAS LPAREN x=ID EQ STAR y=ID RPAREN { AliasDeref(x, y) }
  | ASSERT LPAREN p=PHIExpr RPAREN { Assert(p) } 

ORExpr :
    e1=ORExpr OR e2=ANDExpr { Exp(e1, e2) }
  | e=ANDExpr { e }

ANDExpr :
    e1=ANDExpr AND e2=LTExpr { ANDExp(e1, e2) }
  | e=LTExpr { e1 }

LTExpr :
    e1=BaseExpr LT e2=BaseExpr { LTExp(e1, e2) }
  | e=EQExpr { e }

EQExpr :
    e1=BaseExpr EQ e2=BaseExpr { EQExp(e1, e2) }

BaseExpr :
    i=INTV { ILit (i) }
  | i=ID { i }
  | LPAREN e=ORExpr RPAREN { e }

PHIExpr :
    i=ID { Phi i }

AExpr :
    i=INTV { ILit i }
  | i=ID   { Var i }
  | LPAREN e=Expr RPAREN { e }

 

