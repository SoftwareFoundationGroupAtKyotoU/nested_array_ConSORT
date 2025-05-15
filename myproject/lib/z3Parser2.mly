%{
  open Z3Syntax2
%}

// values
%token <Z.t> INT
%token <float> FLOAT
%token <string> ID

// types
%token TINT TREAL

// structure
%token SAT DEF LPAREN RPAREN
%token MINUS DIV UNDER
%token O C D L H I

%start result
%type <Z3Syntax2.result> result
%%

result:
  | SAT LPAREN defines RPAREN { $3 }
  | SAT LPAREN RPAREN { [] }
;

defines:
| define 
  { [$1] } 
| define defines
  { $1 :: $2 }
;

define:
| LPAREN DEF O i1=int UNDER id=id UNDER pos=pos UNDER i2=int LPAREN RPAREN TREAL v=value RPAREN 
  { Own(Z.to_int i1, id, pos, Z.to_int i2, v) }
| LPAREN DEF C i1=int UNDER H id1=id UNDER id2=id UNDER pos=pos UNDER i2=int LPAREN RPAREN TINT v=value RPAREN 
  { CHigh(Z.to_int i1, id1, id2, pos,Z.to_int i2, v) }
| LPAREN DEF C i1=int UNDER L id1=id UNDER id2=id UNDER pos=pos UNDER i2=int LPAREN RPAREN TINT v=value RPAREN 
  { CLow(Z.to_int i1, id1, id2, pos,Z.to_int i2, v) }
| LPAREN DEF D i1=int UNDER H id=id UNDER pos=pos UNDER i2=int LPAREN RPAREN TINT v=value RPAREN 
  { DHigh(Z.to_int i1, id, pos,Z.to_int i2, v) }
| LPAREN DEF D i1=int UNDER L id=id UNDER pos=pos UNDER i2=int LPAREN RPAREN TINT v=value RPAREN 
  { DLow(Z.to_int i1, id, pos, Z.to_int i2, v) }
;

pos:
| int 
  { Z.to_string $1 }
| id
  { $1 }
| pos UNDER id
  { $1 ^ "_" ^ $3 }
;

value:
| INT 
  { Int($1) }
| LPAREN MINUS INT RPAREN
  { Int(Z.neg $3) } 
| FLOAT
  { Float($1) }
| LPAREN DIV FLOAT FLOAT RPAREN
  { Float($3/.$4) }

int:
  INT { $1 }
;

id:
  ID { $1 }
;
