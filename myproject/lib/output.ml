open Eval
open Typing

(*バッチインタプリタの場合のevalprint*)
let read_eval_print env tyenv inchannel =
  (* 入力からバッファを生成 *)
  let buffer = Lexing.from_channel inchannel in
  let ast = Parser.toplevel Lexer.main buffer in
  let _ = eval_main env ast in
  let (_, ty) = ty_exp tyenv ast in
  print_ast ast

let initial_env =
  Environment.empty

(* Update in 4.4.1 *)
(* tyenvの型が変わったため *)
let initial_tyenv = 
    (Environment.empty)