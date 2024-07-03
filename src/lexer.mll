{
    let reservedWords = [
        (*予約語*)
        ("alias", Parser.ALIAS);
        ("assert", Parser.ASSERT);
        ("else", Parser.ELSE);
        ("ifnp", Parser.IFNP);
        ("in", Parser.IN);
        ("int", Parser.INT);
        ("let", Parser.LET);
        ("alloc", Parser.ALLOC);
        ("ref", Parser.REF);
        ("then", Parser.THEN);
    ]
}

rule main = parse
  (*改行と空白とタブと改ページは無視*)
  [' ' '\009' '\012' '\n']+   { main lexbuf }(*?*)
  | "-"? ['0'-'9']+ {Parser.INTV (int_of_string (Lexing.lexeme lexbuf)) }
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
  | "=" { Parser.EQ }
  | ":" { Parser.COLON }
  | ";" { Parser.SEMI }
  | ":=" { Parser.ASSIGN }
  | "||" { Parser.OR }
  | "&&" { Parser.AND }
  | "_" { Parser.NONDET }
  | "~" { Parser.WAVE }
  | "!" { Parser.NOT }
  | "->" { Parser.RARROW }
  | "," { Parser.COMMMA }
  | "(*" { comment lexbuf; main lexbuf }
  (*コメントの先頭を読んだ際はエントリポイント「コメント」に移ったのちメインに戻ってくる*)
  | ['a'-'z'] ['a'-'z' '0'-'9' '_' '\'']*
      { let id = Lexing.lexeme lexbuf in
        try
          List.assoc id reservedWords(*予約語に含まれている場合は予約語として機能*)
        with
        _ -> Parser.ID id(*予約語でない場合には変数名として扱う*)
      }
  | eof { Parser.EOF }
and comment = parse 
  | "(*"  { comment lexbuf; comment lexbuf }
  (*コメント内でコメントの先頭を読んだ際はエントリポイント「コメント」に移ったのち
  またエントリポイント「コメント」に戻ってくる*)
  | "*)"  { () }
  | _  { comment lexbuf }
  (*コメント中の文字は全て無視する
  またコメントの最後を読んだ際は何も返さないことを()で表現*)