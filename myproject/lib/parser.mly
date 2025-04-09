%{
open Syntax
%}

// value
%token <Z.t> INTV
%token <float> FLOATV
%token <Syntax.id> ID_NAME
%token TRUE FALSE UNITV NONDET

// conditional
%token IFNP IF THEN ELSE

// let binding
%token LET IN EQ

// make array
%token ALLOC COLON

// annotation
%token ALIAS

// aseert expression
%token ASSERT

// binary operator
%token OR AND PLUS MINUS LT GT LEQ GEQ NEQ
%token STAR // multipul and pointer dereference

// unary operator
%token NOT

// Assignment
%token ASSIGN

// connectives
%token SEMI COMMA

// brackets　() {} []
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET

// type
%token TOP RARROW
%token NU INT REF UNIT TOR TAND TIMPLY TNOT

// others # | (end_of_file)
%token HASH BAR EOF

%start toplevel
%type <Syntax.program> toplevel 
%%(*?*)

toplevel : (* プログラムは関数定義と本体式からなる *)
    LBRACE e=Expr RBRACE EOF { ([], e) }
  | f = FunDefs LBRACE e = Expr RBRACE EOF { (f, e) }

FunDefs : (* 関数定義の集合は関数を表す構造体のリスト *)
  | f = FunDef  { [f] } 
  | f1 = FunDef f2 = FunDefs { f1 :: f2 }

FunDef : (* 関数定義 *)
  func_name = ID LPAREN args=IDs RPAREN LBRACKET annotation = Annotation RBRACKET LBRACE func_body = Expr RBRACE 
  { (func_name, args, annotation, func_body) }

IDs: (* 関数の引数名のコンマ区切り *)
  | x = ID { [x] }
  | x = ID COMMA y = IDs { x :: y }

Annotation: // 関数の引数に単純型の情報を付加
  LT args_before_eval = ID_Funtypes GT RARROW LT args_after_eval = ID_Funtypes BAR return_type = Ftype GT 
  { (args_before_eval, args_after_eval, return_type) }

ID_Funtypes: (* 関数の評価前後の引数名と型のコンマ区切り *)
  | x = ID_Funtype { [x] }
  | x = ID_Funtype COMMA y = ID_Funtypes { x :: y }

ID_Funtype: (* 関数の評価前後の引数名と型 *)
| x = ID COLON idtype = Ftype { (RawId(x), idtype) }
| HASH x = ID COLON idtype = Ftype { (HashId(x), idtype) }

Ftype: // プログラム内に記述する型
// | LBRACE NU COLON TINT BAR smtlib RBRACE
//   { FTInt($6) }
| inner_type=Ftype REF LPAREN e1=Expr COMMA e2=Expr COMMA fl=FLOATV RPAREN
  { FTRef(inner_type, e1, e2, fl) }
| INT { FTInt(VarPred) }
| inner_type = Ftype REF { FTRef(inner_type, ENull, ENull, 0.) }

Expr :
  | e=LetExpr{ e }
  | e=IfExpr { e }
  | e=InsertSEMIExpr { e }
  | e=AppExpr { e }
  | e=DerefExpr { e }
  | e=ORExpr {e}
//   | e=IfExpr { e }
//   | e=LetExpr { e }
//   | e1=PreSEMIExpr SEMI e2=Expr { PreSEMIExpr(e1, e2) }
//   | e=AExpr { e }

IfExpr :
    IFNP x=ID THEN t=Expr ELSE e=Expr { IfnpExp (x, t, e) }
  | IF x=Expr THEN t=Expr ELSE e=Expr { IfExp (x, t, e) }

LetExpr :
  | LET x=ID EQ e1 = Expr IN e2 = Expr { Let (x, e1, e2) }
  | LET x=ID EQ ALLOC e1=Expr COLON ty=SimpleTyExpr REF IN e2=Expr { LetAllocExp(x, e1, SRef ( ty ), e2) }
//   | LET x=ID EQ STAR y=ID IN e=Expr { LetDerefExp(x, Var y, e) }
//   | LET x=ID EQ e1=BinOpExpr IN e2=Expr { LetBinOpExp(x, e1, e2) }
//   | LET x=ID EQ e1=Expr IN e2=Expr { LetBindExp(x, e1, e2) }
//   | LET x=ID EQ app=FunCallExpr IN e=Expr { LetFunCall(x, app, e) }

SimpleTyExpr :
    INT { SInt }
  | ty=SimpleTyExpr REF { SRef (ty) }

PlusMinusExpr :
  | x=PlusMinusExpr PLUS y=MultExpr { PlusExp(x, y) }
  | x=PlusMinusExpr MINUS y=MultExpr { MinusExp(x, y) }
  | e=MultExpr { e }

MultExpr :
  | x=MultExpr STAR y=AExpr { MultExp(x, y) }
  | e=AExpr { e }

// FunCallExpr :
//   | i=ID LPAREN v=VarSeq RPAREN { FunCall(i, v)}

// VarSeq :
//     i=ID { [i] }
//   | i=ID COMMA v=VarSeq { i :: v } 

InsertSEMIExpr :
  | x=ID ASSIGN e1=Expr SEMI e2=Expr { Assign(x, e1, e2) }
  | ALIAS LPAREN e1=ID EQ e2=Expr RPAREN SEMI e3=Expr { Alias(Var e1, e2, e3) }
  | ASSERT LPAREN e1=Expr RPAREN SEMI e2=Expr { Assert(e1, e2) } 
  | e1=Expr SEMI e2=Expr { Seq(e1, e2) }
// PreSEMIExpr :
//     x=ID ASSIGN y=ID { Assign(x, y) }
//   | ALIAS LPAREN x=ID EQ y=ID PLUS z=ID RPAREN { AliasAddPtr(x, y, z) }
//   | ALIAS LPAREN x=ID EQ STAR y=ID RPAREN { AliasDeref(x, y) }
//   | ASSERT LPAREN p=LTExpr RPAREN { Assert(p) } 

ORExpr :
    e1=ORExpr OR e2=ANDExpr { OrExp(e1, e2) }
  | e=ANDExpr { e }

ANDExpr :
    e1=ANDExpr AND e2=NotExpr { AndExp(e1, e2) }
  | e=NotExpr { e }

NotExpr :
  | NOT e=NotExpr { e }
  | e=CompareExpr { e }

CompareExpr :
  | e1=PlusMinusExpr EQ e2=PlusMinusExpr { EqExp(e1, e2) }
  | e1=PlusMinusExpr LT e2=PlusMinusExpr { LtExp(e1, e2) }
  | e1=PlusMinusExpr GT e2=PlusMinusExpr { GtExp(e1, e2) }
  | e1=PlusMinusExpr LEQ e2=PlusMinusExpr { LeqExp(e1, e2) }
  | e1=PlusMinusExpr GEQ e2=PlusMinusExpr { GeqExp(e1, e2) }
  | e1=PlusMinusExpr NEQ e2=PlusMinusExpr { NeqExp(e1, e2) }
  | e=PlusMinusExpr{ e }

// LTExpr :
//     e1=BaseExpr LT e2=BaseExpr { BinOp(Lt, e1, e2) }
//   | e=EQExpr { e }

// EQExpr :
//     e1=BaseExpr EQ e2=BaseExpr { BinOp(Eq, e1, e2) }

// BaseExpr :
//     i=INTV { ILit (i) }
//   | i=ID { Var i }
//   | STAR e=BaseExpr { Deref e }
//   | LPAREN e=ORExpr RPAREN { e }

AExpr :
    i=INTV { ILit i }
  | MINUS e=Expr { MinusExp(ILit Z.zero, e) }
  | i=ID   { Var i }
  | LBRACE e=Expr RBRACE { e }
  | TRUE { BLit true }
  | FALSE { BLit false }
  | NONDET { Nondet }
  | UNITV { Unit }

AppExpr :
  | f=ID LPAREN ids=Args RPAREN { AppExp(f, ids) }

Args:
  | e=Expr { [e] }
  | e=Expr COMMA ids=Args { e :: ids }

DerefExpr :
  | STAR x=ID { Deref(x) }

ID :
  | x=ID_NAME { x }
  | NU { "v" } 