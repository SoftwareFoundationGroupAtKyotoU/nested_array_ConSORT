open Eval
open Typing

let read_eval_print env tyenv inchannel =
  (* 入力からバッファを生成 *)
  let buffer = Lexing.from_channel inchannel in
  let (_, exp_ast) = Parser.toplevel Lexer.main buffer in
  let _ = eval_main env exp_ast in
  (* let (_, ty) = ty_exp tyenv ast in *)
  print_ast exp_ast

let initial_env =
  Environment.empty

let initial_tyenv = 
    (Environment.empty)