open Z3Syntax
open SimpleTyping
(* z3から得た反例を次のループに渡すためのモジュール *)

(* 変数と代入すべき値(反例)のリストの組 *)
let cexamples : (id * (value list) ref) list ref = ref []

(* unknownで反例が得られなかった時のための次の具体化するための数字 *)
let num = ref 1

(* unsatで反例が得られたた時の反例の追加 *)
let add_cexample id value =
  try
    let cexample = lookup id !cexamples in
    if not (List.mem value !cexample) then
      cexample := value :: !cexample
  with
    _ -> cexamples := (id, ref [value]) :: !cexamples

(* unknownで反例が得られなかった時用の具体化する数字の追加 *)
let add_sample () =
  (List.iter
  (fun (x, _) -> add_cexample x (Int !num))
  !cexamples);
  num := 1 + !num

(* z3から反例の読み出し *)
let create_cexapmle () =
  let oc_r = open_in "experiment/result_cexample" in
  (* main_intで得られた所有権関数の係数の候補 *)
  let z3res = Z3Parser.result Z3Lexer.read (Lexing.from_channel oc_r) in
  close_in oc_r;
  let rec create_cexapmle_sub z3res =
    match z3res with
    | [] -> ()
    | (id, _,Int i) :: left ->
      if i <> 0 then add_cexample id (Int i); create_cexapmle_sub left
    | (id,_,Float f) :: left ->
      if f <> 0. then add_cexample id (Float f); create_cexapmle_sub left
    | (id,_,Div (f1,f2))  :: left ->
      if f1 <> 0. then add_cexample id (Div (f1,f2)); create_cexapmle_sub left in
      create_cexapmle_sub z3res;