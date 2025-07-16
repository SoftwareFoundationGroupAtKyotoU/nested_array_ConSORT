%{
open Syntax
open SmtlibSyntax
%}

// value
%token <Z.t> INTV
%token <float> FLOATV
%token <Syntax.id> ID_NAME
%token TRUE FALSE UNITV ConstRandInt

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
%token IMMUT

// assume expression
%token ASSUME

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
%token NU INT REF TOR TAND TIMPLY TNOT

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
| LBRACE NU COLON INT BAR sl=Smtlib RBRACE
  { FTInt(sl) } 
| inner_type=Ftype REF LPAREN e1=Expr COMMA e2=Expr COMMA fl=FLOATV RPAREN
  { FTRef(inner_type, e1, e2, fl) }
| INT { FTInt(VarPred) }
| inner_type = Ftype REF { FTRef(inner_type, ENull, ENull, 0.) }

Smtlib:
| LPAREN sl1=Smtlib TOR sl2=Smtlib RPAREN
  { Or(sl1, sl2) }
| LPAREN sl1=Smtlib TAND sl2=Smtlib RPAREN
  { And(sl1, sl2) } 
| LPAREN TIMPLY sl1=Smtlib sl2=Smtlib RPAREN
  { Imply(sl1, sl2) } 
| LPAREN TNOT sl=Smtlib RPAREN
  { Not(sl) } 
| LPAREN sl1=Smtlib EQ sl2=Smtlib RPAREN
  { Eq(sl1, sl2) } 
| LPAREN sl1=Smtlib LT sl2=Smtlib RPAREN
  { Lt(sl1, sl2) }  
| LPAREN sl1=Smtlib GT sl2=Smtlib RPAREN
  { Gt(sl1, sl2) } 
| LPAREN sl1=Smtlib LEQ sl2=Smtlib RPAREN
  { Leq(sl1, sl2) } 
| LPAREN sl1=Smtlib GEQ sl2=Smtlib RPAREN
  { Geq(sl1, sl2) } 
| LPAREN sl1=Smtlib PLUS sl2=Smtlib RPAREN
  { Add(sl1, sl2) } 
| LPAREN sl1=Smtlib MINUS sl2=Smtlib RPAREN
  { Sub(sl1, sl2) } 
| LPAREN sl1=Smtlib STAR sl2=Smtlib RPAREN
  { Mul(sl1, sl2) } 
// | LPAREN DIV smtlib smtlib RPAREN
//   { Div($3, $4) } 
| TOP
  { Id("true") }
| NU
  { Id("v") }
| id=ID
  { FV(id) }
| i=INTV
  { Id(Z.to_string i) }
;

Brackets :
  | LBRACKET e=Expr RBRACKET { [e] }
  | LBRACKET e=Expr RBRACKET b=Brackets {e :: b}

Expr :
  | e=LetExpr{ e }
  | e=IfExpr { e }
  | e=InsertSEMIExpr { e }
  | e=AppExpr { e }
  | e=DerefExpr { e }
  | e=ORExpr {e}

IfExpr :
    IFNP x=ID THEN t=Expr ELSE e=Expr { IfnpExp (x, t, e) }
  | IF x=Expr THEN t=Expr ELSE e=Expr { IfExp (x, t, e) }

LetExpr :
  | LET x=ID EQ e1 = Expr IN e2 = Expr { Let (x, e1, e2) }
  | LET IMMUT x=ID EQ y=ID PLUS e1=Expr IN e2 = Expr { LetImmutAddPtrExp (x, y, e1, e2) }
  | LET x=ID EQ ALLOC e1=Expr COLON ty=Ftype IN e2=Expr { LetAllocExp(x, e1, ty, e2) }

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


InsertSEMIExpr :
  | x=ID ASSIGN e1=Expr SEMI e2=Expr { Assign(x, e1, e2) }
  | ALIAS LPAREN e1=ID EQ e2=Expr RPAREN SEMI e3=Expr { Alias(Var e1, e2, e3) }
  | ASSERT LPAREN e1=Expr RPAREN SEMI e2=Expr { Assert(e1, e2) } 
  | ASSUME LPAREN e1=Expr RPAREN SEMI e2=Expr { Assume(e1, e2) } 
  | e1=Expr SEMI e2=Expr { Seq(e1, e2) }

ORExpr :
    e1=ORExpr OR e2=ANDExpr { OrExp(e1, e2) }
  | e=ANDExpr { e }

ANDExpr :
    e1=ANDExpr AND e2=NotExpr { AndExp(e1, e2) }
  | e=NotExpr { e }

NotExpr :
  | NOT e=NotExpr { NotExp e }
  | e=CompareExpr { e }

CompareExpr :
  | e1=PlusMinusExpr EQ e2=PlusMinusExpr { EqExp(e1, e2) }
  | e1=PlusMinusExpr LT e2=PlusMinusExpr { LtExp(e1, e2) }
  | e1=PlusMinusExpr GT e2=PlusMinusExpr { GtExp(e1, e2) }
  | e1=PlusMinusExpr LEQ e2=PlusMinusExpr { LeqExp(e1, e2) }
  | e1=PlusMinusExpr GEQ e2=PlusMinusExpr { GeqExp(e1, e2) }
  | e1=PlusMinusExpr NEQ e2=PlusMinusExpr { NeqExp(e1, e2) }
  | e=PlusMinusExpr{ e }

AExpr :
    i=INTV { ILit i }
  | MINUS e=AExpr { MinusExp(ILit Z.zero, e) }
  | i=ID   { Var i }
  | LBRACE e=Expr RBRACE { e }
  | TRUE { BLit true }
  | FALSE { BLit false }
  | ConstRandInt { ConstRandInt (BLit true) }
  | ConstRandInt COLON LPAREN x=Expr RPAREN { ConstRandInt x }
  | UNITV { Unit }
  | id=ID ids=Brackets { DerefBracketExp(id, ids) }

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