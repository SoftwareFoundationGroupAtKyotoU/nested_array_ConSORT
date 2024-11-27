open SimpleTyping
open Elaborate
open CollectOwnConstraint
open GenerateSmtlibConstraint
open PrintOwnConstraint
open Z3Syntax

exception Parse_error of Lexing.position * Lexing.position


(* 所有権計算に必要なsmtlibでの宣言(declare)の書き出し 
*)
let rec main_int_declare oc all_cs fun_num iter = 
  if fun_num < 0 then 
    ()
  else
    (let (var_locations, varown_count, fvs, _) = all_cs_to_smtlib all_cs false fun_num in
    (* out_int.smtに所有権計算に必要なsmtlibでの宣言の書き出し，関数評価中の定数係数の宣言 *)
    print_declare oc var_locations fvs fun_num;
    (* out_int.smtに所有権計算に必要なsmtlibでの宣言の書き出し，関数評価前，評価後の定数係数の宣言 *)
    print_declare_begin_and_end oc varown_count fvs fun_num;
    (* 関数ブロックごとに一行区切る *)
    output_string oc "\n";
    (* 次の関数の所有権をsmtlib形式で宣言 *)
    main_int_declare oc all_cs (fun_num-1) iter)

(* 所有権の制約のsmtlib形式（assert）での書き出し 
oc 出力ファイル
all_cs 制約集合
is_unconcrete 変数を具体化するかどうか，所有権推論のFirst phaseかSecond phaseかを表す
flag 現状使ってない
fun_num 関数の通し番号
iter 変数の具体化の範囲
*)
let rec main_int_smtlibs oc all_cs is_unconcrete flag fun_num iter = 
  if fun_num < 0 then 
    ()
  else
    (* n番目の関数を表す組，slsは準smtlib形式の制約のリスト，flagは制約の統合の仕方？ *)
    (let (_, _, fvs, smtlibs) = all_cs_to_smtlib all_cs flag fun_num in
    (* 制約をassertとしてファイルに書き出し，bool_idは関数print_smtlibsの分岐 *)
    print_smtlibs oc smtlibs is_unconcrete fvs (-1) iter;
    (* 関数の制約の間は二行開ける *)
    output_string oc "\n\n";
    (* 次の関数の制約出力へ *)
    main_int_smtlibs oc all_cs is_unconcrete flag (fun_num-1) iter)

(** First phase of the ownershipip inference:
    Generates n_1, ..., n_k and checks the validity of \exists y . phi(n_1, y) /\ ... /\ phi(n_k, y) *)
let generate_constrs file iter = 
  let oc = open_in file in
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
  main_int_declare oc1 all_constrs fun_num iter;
  (* 所有権計算に必要なsmtlibでのassert式の書き出しと
  ヒューリスティクスによるfor all付きの変数の整数値への具体化 *)  
  main_int_smtlibs oc1 all_constrs false false fun_num iter;
  (* 充足可能か調べる *)
  output_string oc1 "(check-sat)\n";
  (* 充足可能な場合に具体的な値を取得 *)
  output_string oc1 "(get-model)\n";
  close_out oc1

(** Second phase of the ownershipip inference:
    Checks the validity of \forall x phi(x, a), where a is the witeness for \exist y obtasined in the first phase. *)
let main_fv file =
  let oc_r1 = open_in file  in
  let oc_r2 = open_in "experiment/result_int" in
  (* プログラムの読み出し *)
  let program = Parser.toplevel Lexer.main (Lexing.from_channel oc_r1) in
  (* main_intで得られた所有権関数の係数の候補 *)
  let z3res = Z3Parser.result Z3Lexer.read (Lexing.from_channel oc_r2) in
  close_in oc_r1; close_in oc_r2;
  let (fdefs, _) = program in
  let n = List.length fdefs in 
  infer_prog_simpleTy program;
  let elaborate_program = elaborate_prog program in
  let all_constrs = collect_program_own_constraints elaborate_program in 
  let oc = open_out "experiment/out_fv.smt2" in
  
  (* 所有権計算に必要なsmtlibでの変数宣言の書き出し *)
  main_int_declare oc all_constrs n 0;
  (* 所有権計算に必要なsmtlibでのassert式の書き出し
  ヒューリスティクスを使わず完全な形の論理式で制約を表す． *)
  (* Printf.eprintf "Error:\n"; *)
  main_int_smtlibs oc all_constrs true false n 0;
  (* Printf.eprintf "Error:\n"; *)
  (* main_intで得られた所有権の係数をassert形式で表現 *)
  print_z3result oc z3res;
  output_string oc "\n\n";
  (* 充足可能か調べる *)
  output_string oc "(check-sat)\n";
  output_string oc "(get-model)\n";
  close_out oc

let print_program file = 
  let oc = open_in file in
  let program = Parser.toplevel Lexer.main (Lexing.from_channel oc) in
  close_in oc;
  infer_prog_simpleTy program;
  (* ASTの精緻化 *)
  let (_,elaborate_program) = elaborate_prog program in
  ()
  (* let oc = open_in file in
  let program = Parser.toplevel Lexer.main (Lexing.from_channel oc) in
  close_in oc;
  let (_, elaborate_program) = elaborate_prog program in
  infer_prog_simpleTy program;
  Util.print_exp elaborate_program *)