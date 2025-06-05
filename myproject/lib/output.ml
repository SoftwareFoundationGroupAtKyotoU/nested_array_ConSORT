open SimpleTyping
open Elaborate
open CollectOwnConstraint
open GenerateSmtlibConstraint
open PrintOwnConstraint
open Z3Syntax
open CHCgenerateSmtlibConstraint
open OwntoCHC
open CHCcollectConstraint
open CHCSyntax

exception Parse_error of Lexing.position * Lexing.position


(* 所有権計算に必要なsmtlibでの宣言(declare)の書き出し 
*)
let rec main_int_declare oc all_cs fun_num = 
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
    main_int_declare oc all_cs (fun_num-1))

let rec main_cexample_declare oc all_cs fun_num z3res= 
  let rec main_cexample_sub fun_num =
    if fun_num < 0 then 
      []
    else
      (print_idx oc fun_num all_cs;
      let (_, _, fvs, smtlibs) = all_cs_to_smtlib all_cs false fun_num in
      List.iter (fun x -> output_string oc (Format.asprintf "(declare-fun %s () Int)\n" x)) fvs;
      let smtlibs' = main_cexample_sub (fun_num-1) in
      smtlibs @ smtlibs') in
  let smtlibs = main_cexample_sub fun_num in
  output_string oc "\n";
  print_cexample oc smtlibs z3res;
  output_string oc "\n"

let main_cexample file =
  let oc = open_in file in
  let oc_r2 = open_in "experiment/result_int" in
  let program = Parser.toplevel Lexer.main (Lexing.from_channel oc) in
  (* main_intで得られた所有権関数の係数の候補 *)
  let z3res = Z3Parser.result Z3Lexer.read (Lexing.from_channel oc_r2) in
  close_in oc; close_in oc_r2;
  let (fdefs, _) = program in
  let fun_num = List.length fdefs in 
  infer_prog_simpleTy program;
  (* ASTの精緻化 *)
  let elaborate_program = elaborate_prog program in
  (* 制約収集 *)
  let all_constrs = collect_program_own_constraints elaborate_program in 

  let oc1 = open_out "experiment/out_cexample.smt2" in
  main_cexample_declare oc1 all_constrs fun_num z3res;

  output_string oc1 "\n\n";
  (* 充足可能か調べる *)
  output_string oc1 "(check-sat)\n";
  (* 充足可能な場合に具体的な値を取得 *)
  output_string oc1 "(get-model)\n";
  (* output_string oc1 "(get-unsat-core)\n"; *)
  close_out oc1

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
    (* let fvs' = find_idx_vars_be varown_count fun_num in
    let fvs'' = find_idx_vars var_locations fun_num in
    let fvs = fvs@fvs'@fvs'' in *)
    (* Format.printf "%d\n" fun_num; *)
    (* let _ = List.map (fun x -> Format.printf "%s\n" x) fvs' in
    let _ = List.map (fun x -> Format.printf "%s\n" x) fvs'' in  *)
    let smtlibs = if is_unconcrete then smtlibs else [SmtlibSyntax.Ands smtlibs] in
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
  main_int_declare oc1 all_constrs fun_num;
  (* 所有権計算に必要なsmtlibでのassert式の書き出しと
  ヒューリスティクスによるfor all付きの変数の整数値への具体化 *)  
  main_int_smtlibs oc1 all_constrs false false fun_num iter;
  (* 充足可能か調べる *)
  output_string oc1 "(check-sat-using psmt)\n";
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

  (* output_string oc "(set-option :produce-unsat-cores true)\n"; *)
  
  (* 所有権計算に必要なsmtlibでの変数宣言の書き出し *)
  main_int_declare oc all_constrs n;
  (* 所有権計算に必要なsmtlibでのassert式の書き出し
  ヒューリスティクスを使わず完全な形の論理式で制約を表す． *)
  main_int_smtlibs oc all_constrs true false n 0;
  (* main_intで得られた所有権の係数をassert形式で表現 *)
  print_z3result oc z3res;
  output_string oc "\n\n";
  (* 充足可能か調べる *)
  output_string oc "(check-sat)\n";
  output_string oc "(get-model)\n";
  (* output_string oc "(get-unsat-core)\n"; *)
  close_out oc

let rec main_sat_ans_sub oc all_cs fun_num iter z3_res total_fun_num = 
  let fun_num' = total_fun_num - fun_num in
  if fun_num < 0 then 
    ()
  else
    (let (_, varown_count, fvs, _) = all_cs_to_smtlib all_cs false fun_num' in
    (* out_int.smtに所有権計算に必要なsmtlibでの宣言の書き出し，関数評価前，評価後の定数係数の宣言 *)
    print_sat_ans oc varown_count fvs fun_num' z3_res all_cs;
    (* 関数ブロックごとに一行区切る *)
    output_string oc "\n";
    (* 次の関数の所有権をsmtlib形式で宣言 *)
    main_sat_ans_sub oc all_cs (fun_num-1) iter z3_res total_fun_num)

let main_sat_ans file =
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
    let oc = open_out "experiment/out_sat_ans.smt2" in
  
    let z3res' = List.map (fun (id , _ , value) -> (id, value)) z3res in
    output_string oc (file ^ " ->\n");
    (* 所有権計算に必要なsmtlibでの変数宣言の書き出し *)
    main_sat_ans_sub oc all_constrs n 0 z3res' n;
    close_out oc

(* refinement検査のための変数の書き出し *)
let rec main_chc_sub_declare oc all_chcs n = 
if n < 0 then 
  ()
else
  (let oc_r2 = open_in "experiment/result" in
  (* let z3res = Z3Parser2.result Z3Lexer2.read (Lexing.from_channel oc_r2) in *)
  close_in oc_r2;
  (* 
  id_count_chc: (変数id, (プログラムの位置l, ifel))のリスト
  varpred_count: 篩型の述語，(所有範囲の下限，所有範囲の上限，所有権の値)のリスト
  fvs: 自由整数変数(#付きの整数引数)
  ss: 篩型の制約 *)
  let (id_count, varpred_count, fvs, _) = all_cs_to_smtlib_chc all_chcs n in
  (* 所有権の基本的な制約，所有範囲の範囲内で篩型が満たされるという制約 *)
  (* let args_own_sls = ownexp_to_ownchc varpred_count n in *)
  (*  *)
  (* let own_sls = collect_ownchc z3res n fvs in  *)

  (* 制約をファイルに書き出し　daclare-fun部分 
  intpred_env:篩型の環境 *)
  print_declare_chc_int oc !CHCSyntax.intpred_env n;
  print_declare_chc oc id_count fvs n;
  print_declare_varpred oc varpred_count n;
  output_string oc "\n";
  main_chc_sub_declare oc all_chcs (n-1))

let rec main_chc_sub oc all_chcs n = 
  (* 前半はmain_chc_sub_declare *)
  if n < 0 then 
    ()
  else
    (let oc_r2 = open_in @@ "experiment/result" in
    let z3res = Z3Parser2.result Z3Lexer2.read (Lexing.from_channel oc_r2) in
    close_in oc_r2;
    (* 
      id_count_chc: (変数id, (プログラムの位置l, ifel))のリスト
      varpred_count: 篩型の述語，(所有範囲の下限，所有範囲の上限，所有権の値)のリスト
      fvs: 自由整数変数(#付きの整数引数)
      ss: 篩型の制約 *)
    let (_, varpred_count, fvs, sls) = all_cs_to_smtlib_chc all_chcs n in
    let args_own_sls = ownexp_to_ownchc varpred_count n in
    let (_, chcs, _) = (List.nth all_chcs n) in
    let own_sls = List.concat_map (fun x -> outer_constrs z3res n fvs x) chcs in 

    (* 制約をファイルに書き出し　assert部分 *)
    print_smtlibs oc sls true fvs n 0; 
    output_string oc "\n";
    print_smtlibs oc args_own_sls true fvs n 0; 
    output_string oc "\n";
    print_smtlibs oc own_sls true fvs n 0; 
    output_string oc "\n\n";
    main_chc_sub oc all_chcs (n-1))

(** Main procedure for the refinement inference *)
let main_chc file =
  let oc_r1 = open_in file  in
  let prog = Parser.toplevel Lexer.main (Lexing.from_channel oc_r1) in
  close_in oc_r1; 
  let (fdefs, _) = prog in
  let n = List.length fdefs in 
  infer_prog_simpleTy prog;
  (* 関数名，CHCの制約を表すデータ型，最後に評価されうる式の組 *)
  let all_chcs = chc_collect_prog (elaborate_prog prog) in 

  let oc = open_out "experiment/out_chc.smt2" in
  output_string oc "(set-logic HORN)\n\n\n";
  main_chc_sub_declare oc all_chcs n;
  main_chc_sub oc all_chcs n;
  output_string oc "(check-sat)\n";
  output_string oc "(get-model)\n";
  close_out oc
  

let print_program file = 
  let oc = open_in file in
  let program = Parser.toplevel Lexer.main (Lexing.from_channel oc) in
  close_in oc;
  infer_prog_simpleTy program;
  let (fdef, main) = elaborate_prog program in
  let _ = List.map (fun (_, _, _, exp) -> Util.print_exp exp; print_string "\n\n") fdef in
  Util.print_exp main
  (* let oc = open_in file in
  let program = Parser.toplevel Lexer.main (Lexing.from_channel oc) in
  close_in oc;
  let (_, elaborate_program) = elaborate_prog program in
  infer_prog_simpleTy program;
  Util.print_exp elaborate_program *)