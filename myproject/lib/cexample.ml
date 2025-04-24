open Z3Syntax
open SimpleTyping
(* z3から得た反例を次のループに渡すためのモジュール *)

(* 変数と代入すべき値(反例)のリストの組 *)
let cexamples : (id * (value list) ref) list ref = ref []

(* unknownで反例が得られなかった時のための次の具体化するための数字 *)
let num = ref (Z.of_int 2)

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
  num := Z.add Z.one !num

(* z3から反例の読み出し *)
let create_cexapmle () =
  let oc_r = open_in "experiment/result_cexample" in
  let oc_w = open_out_gen [Open_append; Open_creat] 0o666 "experiment/result_cex_all" in
  (* main_intで得られた所有権関数の係数の候補 *)
  let z3res = Z3Parser.result Z3Lexer.read (Lexing.from_channel oc_r) in
  close_in oc_r;
  let rec create_cexapmle_sub z3res =
    match z3res with
    | [] -> ()
    | (id, _,Int i) :: left ->
      if not (Z.equal i Z.one) 
      then 
        add_cexample id (Int i); 
        create_cexapmle_sub left;
        output_string oc_w (Format.sprintf "{%s:%d} " id (Z.to_int i));
    | (id,_,Float f) :: left ->
      if f <> 0. then add_cexample id (Float f); create_cexapmle_sub left
    | (id,_,Div (f1,f2))  :: left ->
      if f1 <> 0. then add_cexample id (Div (f1,f2)); create_cexapmle_sub left in
      create_cexapmle_sub z3res;
      output_string oc_w "\n";
      close_out oc_w;