%{
  open Z3Syntax
%}

// values
%token <Z.t> INT
%token <float> FLOAT
%token <string> ID

// types
%token TINT TREAL MINUS DIV

// structure
%token SAT DEF LPAREN RPAREN

%start result
%type <Z3Syntax.result> result
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
| LPAREN DEF id LPAREN RPAREN TREAL exp RPAREN 
  { ($3, "Real", $7) }
| LPAREN DEF id LPAREN RPAREN TINT exp RPAREN 
  { ($3, "Int", $7) }
;

exp:
| INT 
  { Int($1) }
| LPAREN MINUS INT RPAREN
  { Int(Z.neg $3) } 
| FLOAT
  { Float($1) }
| LPAREN MINUS FLOAT RPAREN
  { Float(-.$3) } 
| LPAREN DIV FLOAT FLOAT RPAREN
  { Div($3, $4) }
| LPAREN MINUS LPAREN DIV FLOAT FLOAT RPAREN RPAREN
  { Div(-.$5, $6) }

id:
  ID { $1 }
;
