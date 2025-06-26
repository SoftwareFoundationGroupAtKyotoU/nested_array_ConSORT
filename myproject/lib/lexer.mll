{
    let reservedWords = [
        (*予約語*)
        ("alias", Parser.ALIAS);
        ("assert", Parser.ASSERT);
        ("assume", Parser.ASSUME);
        ("else", Parser.ELSE);
        ("ifnp", Parser.IFNP);
        ("if", Parser.IF);
        ("in", Parser.IN);
        ("int", Parser.INT);
        ("let", Parser.LET);
        ("alloc", Parser.ALLOC);
        ("ref", Parser.REF);
        ("then", Parser.THEN);
        ("true", Parser.TRUE);
        ("false", Parser.FALSE);
        ("or", Parser.TOR);
        ("and", Parser.TAND);
        ("not", Parser.TNOT);
        ("T", Parser.TOP);
        ("v", Parser.NU);
    ]
}

rule main = parse
  (*改行と空白とタブと改ページは無視*)
  [' ' '\009' '\012' '\n']+   { main lexbuf }(*?*)
  | "-"? ['0'-'9']+ {Parser.INTV (Z.of_string (Lexing.lexeme lexbuf)) }
  | '-'? ['0'-'9']+ '.' ['0' - '9']* {Parser.FLOATV (float_of_string (Lexing.lexeme lexbuf)) }
  | "()" { Parser.UNITV }
  | "(" { Parser.LPAREN }
  | ")" { Parser.RPAREN }
  | "{" { Parser.LBRACE }
  | "}" { Parser.RBRACE }
  | "[" { Parser.LBRACKET } 
  | "]" { Parser.RBRACKET }
  | "+" { Parser.PLUS }
  | "-" { Parser.MINUS }
  | "*" { Parser.STAR }
  | "<" { Parser.LT }
  | ">" { Parser.GT }
  | "<=" { Parser.LEQ }
  | ">=" { Parser.GEQ }
  | "=" { Parser.EQ }
  | "!=" { Parser.NEQ }
  | ":" { Parser.COLON }
  | ";" { Parser.SEMI }
  | ":=" { Parser.ASSIGN }
  | "||" { Parser.OR }
  | "&&" { Parser.AND }
  | "_" { Parser.ConstRandInt }
  | "!" { Parser.NOT }
  | "->" { Parser.RARROW }
  | "=>" { Parser.TIMPLY }
  | "," { Parser.COMMA }
  | "|" { Parser.BAR }
  | "#" { Parser.HASH }
  | "/*" { comment lexbuf; main lexbuf }
  (*コメントの先頭を読んだ際はエントリポイント「コメント」に移ったのちメインに戻ってくる*)
  | ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_' '\'']*
      { let id = Lexing.lexeme lexbuf in
        try
          List.assoc id reservedWords(*予約語に含まれている場合は予約語として機能*)
        with
        _ -> Parser.ID_NAME id(*予約語でない場合には変数名として扱う*)
      }
  | eof { Parser.EOF }
and comment = parse 
  | "/*"  { comment lexbuf; comment lexbuf }
  (*コメント内でコメントの先頭を読んだ際はエントリポイント「コメント」に移ったのち
  またエントリポイント「コメント」に戻ってくる*)
  | "*/"  { () }
  | _  { comment lexbuf }
  (*コメント中の文字は全て無視する
  またコメントの最後を読んだ際は何も返さないことを()で表現*)