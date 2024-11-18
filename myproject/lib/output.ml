open SimpleTyping
open Elaborate
open CollectOwnConstraint
open GenerateSmtlibConstraint
open PrintOwnConstraint
open Printf

exception Parse_error of Lexing.position * Lexing.position


(* 所有権計算に必要なsmtlibでの宣言の書き出し 
bool_id使ってない？
*)
let rec main_int_sub_declare oc all_cs bool_id fun_num iter = 
  if fun_num < 0 then 
    ()
  else
    (let (var_locations, varown_count, fvs, smtlibs) = all_cs_to_smtlib all_cs false fun_num in
    (* out_int.smtに所有権計算に必要なsmtlibでの宣言の書き出し，関数評価中の定数係数の宣言 *)
    print_declare oc var_locations fvs fun_num;
    (* out_int.smtに所有権計算に必要なsmtlibでの宣言の書き出し，関数評価前，評価後の定数係数の宣言 *)
    print_declare_begin_and_end oc varown_count fvs fun_num;
    (* 関数ブロックごとに一行区切る *)
    output_string oc "\n";
    (* 次の関数の所有権をsmtlib形式で宣言 *)
    main_int_sub_declare oc all_cs bool_id (fun_num-1) iter)

(** First phase of the ownershipip inference:
    Generates n_1, ..., n_k and checks the validity of \exists y . phi(n_1, y) /\ ... /\ phi(n_k, y) *)
let generate_constrs file iter = 
  let oc = open_in file in
  try
    let program = Parser.toplevel Lexer.main (Lexing.from_channel oc) in
    close_in oc;
    let (fdefs, _) = program in
    let fun_num = List.length fdefs in 
    infer_prog_simpleTy program;
    (* ASTの精緻化 *)
    let elaborate_program = elaborate_prog program in
    (* 制約収集 *)
    let all_constrs = collect_program_own_constraints elaborate_program in 

    let oc1 = open_out "experiment/out_int.smt2" in
    (* 所有権計算に必要なsmtlibでの変数宣言の書き出し *)
    main_int_sub_declare oc1 all_constrs false fun_num iter
  with
  | Parse_error (start_p, _) ->
    printf "Parse error at line %d, column %d\n"
      start_p.Lexing.pos_lnum
      (start_p.Lexing.pos_cnum - start_p.Lexing.pos_bol + 1)