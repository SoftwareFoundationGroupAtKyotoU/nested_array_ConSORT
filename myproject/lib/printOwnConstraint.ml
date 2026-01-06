open Util
open Format
open SmtlibSyntax
open SimpleTyping
open Z3Syntax
open OwnConstraintSyntax
open Cexample

(* 代入，読み出しにより変則的な所有権の形をしているidのリスト *)
let eq0_list : id list ref = ref []

(* z3に書き出す時にassertに名前を付ける用
unsatの時にどの制約同士が矛盾してるかを出してくれる *)
let serial_num = ref 0

  (* 文字列の末尾の削除 *)
let remove_suffix s n =
  let s_len = String.length s in
  if n > s_len then s
  else
    String.sub s 0 (s_len - n)

(* 所有権計算に必要なsmtlibでの変数宣言 *)
let rec print_declare oc var_locations fun_num =
  let formatter = formatter_of_out_channel oc in
  let rec nested_ref_declare id pos branch_trace depth fvs = 
    if depth < 1 then ()
    else
    (* 所有権を表す変数の宣言　o_(関数のシリアル番号)_(参照変数名)_(関数内での位置を表す整数)_(then or else)_depth *)
      (fprintf formatter "(declare-fun o_%d_%s_%d%a_%d () Real)\n" fun_num id pos pp_branch_trace branch_trace depth;
      (* 下限の係数を宣言 *)
      print_declare_c formatter fvs "_l" id pos branch_trace fun_num depth;
      (* 所有権を表す下限の一次式の切片の宣言 
      [d + c1*x1 + ..., d' + c1'*x1 + ...] -> o のd
      d_(関数のシリアル番号)_l_(参照変数名)_(関数内での位置を表す整数)_(then or else)_depth *)
      let intercept_l = asprintf "d_%d__l_%s_%d%a_%d" fun_num id pos pp_branch_trace branch_trace depth in
      fprintf formatter "(declare-fun %s () Int)\n" intercept_l;
      (* 上限の係数を宣言 *)
      print_declare_c formatter fvs "_h" id pos branch_trace fun_num depth;
      (* 所有権を表す上限の一次式の切片の宣言 
      d_(関数のシリアル番号)_h_(参照変数名)_(関数内での位置を表す整数)_(then or else)_depth *)
      let intercept_h = asprintf "d_%d__h_%s_%d%a_%d" fun_num id pos pp_branch_trace branch_trace depth in
      fprintf formatter "(declare-fun %s () Int)\n" intercept_h;
      (* 配列の添え字を表す変数の宣言
      i_(関数のシリアル番号)_(参照変数名)_(関数内での位置を表す整数)_(depth)th_(then or else) *)
      (* fprintf formatter "(declare-fun i_%d_%s_%d_%dth%a () Int)\n" fun_num id pos depth pp_branch_trace branch_trace; *)
      let idx = asprintf "i_%d_%s_%dth" fun_num id depth in
      let fvs' = idx::fvs in
      nested_ref_declare id pos branch_trace (depth-1) fvs') in
  (List.iter
    (fun (id,(pos,branch_trace, simpleTy, fvs)) ->
      let depth = ref_depth simpleTy in 
      nested_ref_declare id pos branch_trace depth fvs) var_locations);
(* 所有範囲の上限または下限の宣言 *)
and print_declare_c formatter fvs l_or_h id pos branch_trace fun_num depth =
  List.iter
    (fun fv ->
      (* smtlibに渡す参照の範囲の上限下限を表す一次式のうち，環境の自由変数の係数定数を宣言
      [d + c1*x1 + ..., d' + c1'*x1 + ...] -> o のc1やc1'
      c_(関数のシリアル番号)_(l(下限) or h(上限))_(自由変数名)_(参照変数名)_(関数内での位置を表す整数)_(if or el)_(参照の深さ) *)
      let coeff = asprintf "c_%d_%s_%s_%s_%d%a_%d"
       fun_num l_or_h fv id pos pp_branch_trace branch_trace depth in
      fprintf formatter "(declare-fun %s () Int)\n" coeff;
      ) fvs

let rec print_lim oc var_locations fun_num =
  let formatter = formatter_of_out_channel oc in
  let rec nested_ref_declare id pos branch_trace depth fvs = 
    if depth < 1 then ()
    else
      (
      (* 下限の係数を宣言 *)
      print_declare_c formatter fvs "_l" id pos branch_trace fun_num depth;
      (* 上限の係数を宣言 *)
      print_declare_c formatter fvs "_h" id pos branch_trace fun_num depth;
      let idx = asprintf "i_%d_%s_%dth" fun_num id depth in
      let fvs' = idx::fvs in
      nested_ref_declare id pos branch_trace (depth-1) fvs') in
  (List.iter
    (fun (id,(pos,branch_trace, simpleTy, fvs)) ->
      let depth = ref_depth simpleTy in 
      nested_ref_declare id pos branch_trace depth fvs) var_locations);
(* 所有範囲の上限または下限の宣言 *)
and print_declare_c formatter fvs l_or_h id pos branch_trace fun_num depth =
  List.iter
    (fun fv ->
      (* smtlibに渡す参照の範囲の上限下限を表す一次式のうち，環境の自由変数の係数定数を宣言
      [d + c1*x1 + ..., d' + c1'*x1 + ...] -> o のc1やc1'
      c_(関数のシリアル番号)_(l(下限) or h(上限))_(自由変数名)_(参照変数名)_(関数内での位置を表す整数)_(if or el)_(参照の深さ) *)
      let coeff = asprintf "c_%d_%s_%s_%s_%d%a_%d"
        fun_num l_or_h fv id pos pp_branch_trace branch_trace depth in
      fprintf formatter "(assert (and (<= (- 1) %s) (<= %s 1)))\n" coeff coeff;
      ) fvs

(* smtlib形式で変数宣言を行う
関数評価前と評価後の所有権の範囲を表す
print_declareと大体同じ *)
let rec print_lim_begin_and_end oc varown_count fvs fun_num =
  let formatter = formatter_of_out_channel oc in
  let rec nested_ref_declare_begin_and_end id b_or_e depth fvs = 
    if depth < 1 then ()
    else
       (print_declare_b_and_e_c formatter fvs "_l" id b_or_e fun_num depth;
       print_declare_b_and_e_c formatter fvs "_h" id b_or_e fun_num depth;
       let idx = asprintf "i_%d_%s_%dth" fun_num id depth in
       let fvs' = idx::fvs in
       nested_ref_declare_begin_and_end id b_or_e (depth-1) fvs');
     in
  (List.iter
    (fun (id,b_or_e,fun_num',depth) ->
       if fun_num' = fun_num then
        nested_ref_declare_begin_and_end id b_or_e depth fvs
       else 
         ()
       ) varown_count);
(* 関数評価前，評価後の上限下限の定数係数の宣言 *)
and print_declare_b_and_e_c formatter fvs l_or_h id b_or_e fun_num depth =
  List.iter
  (* smtlibに渡す参照の範囲の上限下限を表す一次式のうち，環境の自由変数の係数定数を宣言
      [d + c1*x1 + ..., d' + c1'*x1 + ...] -> o のc1やc1'
      c_(関数のシリアル番号)_(l(下限) or h(上限))_(自由変数名)_(参照変数名)_(b(評価前) or e(評価後))_(参照の深さ) *)
    (fun fv ->
      let coeff = asprintf "c_%d_%s_%s_%s_%s_%d" fun_num l_or_h fv id b_or_e depth in
      fprintf formatter "(assert (and (<= (- 1) %s) (<= %s 1)))\n" coeff coeff;
       ) fvs


(* smtlib形式で変数宣言を行う
関数評価前と評価後の所有権の範囲を表す
print_declareと大体同じ *)
let rec print_declare_begin_and_end oc varown_count fvs fun_num =
  let formatter = formatter_of_out_channel oc in
  let rec nested_ref_declare_begin_and_end id b_or_e depth fvs = 
    if depth < 1 then ()
    else
      (* 所有権を表す変数の宣言　o_(関数のシリアル番号)_(参照変数名)_(b(評価前) or e(評価後)) *)
      (fprintf formatter "(declare-fun o_%d_%s_%s_%d () Real)\n" fun_num id b_or_e depth;
       (* 下限の係数を宣言 *)
       print_declare_b_and_e_c formatter fvs "_l" id b_or_e fun_num depth;
       (* 所有権を表す下限の一次式の切片の宣言 *)
       fprintf formatter "(declare-fun d_%d__l_%s_%s_%d () Int)\n" fun_num id b_or_e depth;
       (* 上限の係数を宣言 *)
       print_declare_b_and_e_c formatter fvs "_h" id b_or_e fun_num depth;
       (* 所有権を表す上限の一次式の切片の宣言 *)
       fprintf formatter "(declare-fun d_%d__h_%s_%s_%d () Int)\n" fun_num id b_or_e depth;
       (* 配列の添え字を表す変数の宣言 *)
       (* fprintf formatter "(declare-fun i_%d_%s_%s_%dth () Int)\n" fun_num id b_or_e depth; *)
       let idx = asprintf "i_%d_%s_%dth" fun_num id depth in
       let fvs' = idx::fvs in
       nested_ref_declare_begin_and_end id b_or_e (depth-1) fvs');
     in
  (List.iter
    (fun (id,b_or_e,fun_num',depth) ->
       if fun_num' = fun_num then
        nested_ref_declare_begin_and_end id b_or_e depth fvs
       else 
         ()
       ) varown_count);
(* 関数評価前，評価後の上限下限の定数係数の宣言 *)
and print_declare_b_and_e_c formatter fvs l_or_h id b_or_e fun_num depth =
  List.iter
  (* smtlibに渡す参照の範囲の上限下限を表す一次式のうち，環境の自由変数の係数定数を宣言
      [d + c1*x1 + ..., d' + c1'*x1 + ...] -> o のc1やc1'
      c_(関数のシリアル番号)_(l(下限) or h(上限))_(自由変数名)_(参照変数名)_(b(評価前) or e(評価後))_(参照の深さ) *)
    (fun fv ->
      fprintf formatter "(declare-fun c_%d_%s_%s_%s_%s_%d () Int)\n" fun_num l_or_h fv id b_or_e depth;
       ) fvs

(* listから重複を除いたリストを返す *)
let rec list_to_set li res = 
  match li with
  | [] -> res 
  | [x] -> if List.mem x res then res else x :: res
  | x :: li' ->
    let res' = list_to_set li' res in
    if List.mem x res' then res' else x :: res' 

let binop_smtlib_to_string oc smtlib =
match smtlib with
| Or _ -> output_string oc "or "
| And _ -> output_string oc "and "
| Imply _ -> output_string oc "=> "
| Eq _ -> output_string oc "= "
| Lt _ ->  output_string oc "< "
| Gt _ -> output_string oc "> "
| Leq _ ->  output_string oc "<= "
| Geq _ -> output_string oc ">= "
| Add _ -> output_string oc "+ "
| Sub _ -> output_string oc "- "
| Mul _ -> output_string oc "* " 
| Div _ -> output_string oc "div " 
| _ -> raise (Error "binop_smtlib_to_string error") 

(* 準smtlibの制約をちゃんとしたsmtlibの制約にしてファイルに書き出す関数 
  slはsmtlibの制約
mapは自由変数から整数への割り当て
numは篩型の識別番号？
*)
let rec print_smtlib oc sl map num = 
  match sl with 
  | Or (s1,s2) | And (s1,s2) | Imply (s1,s2)| Eq (s1,s2) | Lt (s1,s2) 
  | Gt (s1,s2) | Leq (s1,s2) | Geq (s1,s2) | Add (s1,s2)| Sub (s1,s2) | Mul (s1,s2) | Div (s1,s2) -> 
    (output_string oc "(";
     binop_smtlib_to_string oc sl;
     print_smtlib oc s1 map num;
     output_string oc " ";
     print_smtlib oc s2 map num;
     output_string oc ")")
  | Not s -> 
    (output_string oc "(not ";
     print_smtlib oc s map num;
     output_string oc ")")
  | FV id | Id id-> 
    (try
      let n = lookup id map in
      output_string oc (string_of_int n)
    with Error _ -> output_string oc id)    
  | IntPred (id1,ids) -> 
    (output_string oc ("(P" ^ string_of_int num ^ "_" ^ id1);
     List.iter
       (fun id -> 
          output_string oc (" " ^ id)) ids;
    output_string oc ")")
  | IntVarPred (num',id1,ids) -> 
    (output_string oc ("(P" ^ (string_of_int num') ^ "_" ^ id1);
     List.iter
       (fun id -> 
          output_string oc (" " ^ id)) ids;
    output_string oc ")")
  | PtrPred (id,l,i_sl,ids, var_name) -> 
    (output_string oc ("(P" ^ (string_of_int num) ^ "_" ^ id ^ "_" ^ l ^ " ");
     List.iter (fun i -> print_smtlib oc i map num; output_string oc " ") i_sl;
     output_string oc var_name;
     List.iter
       (fun id -> 
          output_string oc (" " ^ id)) ids;
     output_string oc ")")
  | PtrVarPred (num',id,be,i_sl,ids) -> 
    (output_string oc ("(P" ^ (string_of_int num') ^ "_" ^ id ^ "_" ^ be ^ " ");
     List.iter (fun i -> print_smtlib oc i map num; output_string oc " ") i_sl;
     output_string oc " v";
     List.iter
       (fun id -> 
          output_string oc (" " ^ id)) ids;
     output_string oc ")")
  | VarPred ->
    output_string oc "Pvar"
  | True ->
    output_string oc "true"
  | Ands smtlibs -> 
    match smtlibs with
    | [] -> output_string oc "true"
    | sl :: [] -> print_smtlib oc sl map num
    | _ -> 
      (output_string oc "(and";
       List.iter 
         (fun sl ->
            output_string oc " ";
            print_smtlib oc sl map num) smtlibs;
       output_string oc ")")

(* 反例出力に使うsmtlibの出力関数 *)
let rec print_smtlib' oc sl map num z3_res = 
match sl with 
| Or (s1,s2) | And (s1,s2) | Imply (s1,s2)| Eq (s1,s2) | Lt (s1,s2) 
| Gt (s1,s2) | Leq (s1,s2) | Geq (s1,s2) | Add (s1,s2)| Sub (s1,s2) | Mul (s1,s2) | Div (s1,s2) -> 
  (output_string oc "(";
  binop_smtlib_to_string oc sl;
  print_smtlib' oc s1 map num z3_res;
  output_string oc " ";
  print_smtlib' oc s2 map num z3_res;
  output_string oc ")")
| Not s -> 
  (output_string oc "(not ";
    print_smtlib' oc s map num z3_res;
    output_string oc ")")
| FV fv -> 
  (try
    let n = lookup fv map in
    output_string oc (string_of_int n)
  with Error _ -> output_string oc fv)
| Id id -> 
    print_z3result_value oc z3_res id
| True ->
  output_string oc "true"
| Ands smtlibs -> 
  (match smtlibs with
  | [] -> output_string oc "true"
  | sl :: [] -> print_smtlib' oc sl map num z3_res
  | _ -> 
    (output_string oc "(and";
      List.iter 
        (fun sl ->
          output_string oc " ";
          print_smtlib' oc sl map num z3_res) smtlibs;
      output_string oc ")"))
| _ -> ()

let rec idx_of_smtlib sl =
  match sl with 
  | Or (s1,s2) | And(s1, s2) | Imply (s1,s2) | Eq (s1,s2) | Lt (s1,s2) 
  | Gt (s1,s2) | Leq (s1,s2) | Geq (s1,s2) | Add (s1,s2) | Sub (s1,s2) | Mul (s1,s2) | Div (s1,s2) -> 
    (idx_of_smtlib s1) @ (idx_of_smtlib s2)
  | Not s -> 
    idx_of_smtlib s
  (* | Div (s1,s2) -> 
    (fvs_of_smtlib s1) @ (fvs_of_smtlib s2) *)
  | Id id -> 
    if Syntax.starts_with "i_" id then [id] else []
  | FV id -> 
    if Syntax.starts_with "i_" id then [id] else []
  | Ands ss ->
    List.concat (List.map idx_of_smtlib ss)
  | _ -> 
    []
  
(* 自由変数を具体化するための関数 *)
let print_concrete oc fv =
  try
    let cexample = lookup fv !cexamples in
    let rec print_concrete_sub cexample =
      match cexample with
      | [] ->
        output_string oc (asprintf "(= %s %a)" fv pp_value (Int Z.one));
      | value :: left ->
        output_string oc (asprintf "(or (= %s %a) " fv pp_value value);
        print_concrete_sub left;
        output_string oc (asprintf ")")
      in print_concrete_sub !cexample
  with
  _ -> output_string oc (asprintf "(= %s %a) " fv pp_value (Int Z.one))

(* smtlibの制約をファイルに書き出し
oc 書き出し先
smtlibs 制約
is_unconcrete 自由変数を具体化するかどうか falseで具体化 trueで制約をそのまま書き出し
fvs 所有権termが依存できる自由変数
num 篩型用の数字
iter 自由変数を具体化する値の範囲 *)
let rec print_smtlibs oc smtlibs is_unconcrete unsat_core_flag num =
  if is_unconcrete then
    List.iter (print_smtlibs_sub oc unsat_core_flag num) smtlibs
  else
      List.iter
      (fun sl -> 
        let idxs = list_to_set (idx_of_smtlib sl) [] in
        let fvs = list_to_set (fvs_of_smtlib sl @ idxs) [] in
        if fvs = [] then
          (output_string oc "(assert ";
          print_smtlib oc sl [] num; 
          output_string oc ")\n")
        else
          (output_string oc "(assert (forall (";
          output_string oc (make_args fvs);
          output_string oc ") ";
          List.iter 
          (fun fv -> 
            output_string oc "(=> ";
            print_concrete oc fv)
          fvs;
          print_smtlib oc sl [] num; 
          List.iter 
          (* (fun _ -> output_string oc (asprintf "))" )) *)

          (fun _ -> output_string oc (asprintf ")" ))
          fvs;
          output_string oc "))\n"))
      (* (fun sl -> 
        let idxs = list_to_set (idx_of_smtlib sl) [] in
        let fvs = list_to_set (fvs_of_smtlib sl @ idxs) [] in
        if fvs = [] then
          (output_string oc "(assert (! ";
          (* smtlibの制約部分の記述 *)
            print_smtlib oc sl [] num; 
            output_string oc (" :named sl" ^ (string_of_int !serial_num) ^ "))\n");
            serial_num := !serial_num + 1)
        else
          (output_string oc "(assert (! (forall (";
          output_string oc (make_args fvs);
          output_string oc ") ";
          List.iter 
          (fun fv -> output_string oc (asprintf "(=> (<= -%d %s) (=> (<= %s %d) " (iter+1) fv fv (iter+1)))
          fvs;
          print_smtlib oc sl [] num; 
          List.iter 
          (fun _ -> output_string oc (asprintf "))" ))
          fvs;
          output_string oc (") :named sl" ^ (string_of_int !serial_num) ^ "))\n");
          serial_num := !serial_num + 1)) *)
      smtlibs
and print_smtlibs_iter oc smtlibs is_unconcrete unsat_core_flag num iter =
  if is_unconcrete then
    List.iter (print_smtlibs_sub oc unsat_core_flag num) smtlibs
  else
    let rec range m n =
      if m > n then []
      else m :: range (m + 1) n
    in
    let rec generate_combinations fvs int_range =
      match fvs with
      | [] -> [[]]
      | hd :: tl ->
        let combinations_hd = List.map (fun x -> (hd, x)) int_range in
        let combinations_tl = generate_combinations tl int_range in
        List.flatten (List.map (fun a -> List.map (fun b -> a :: b) combinations_tl) combinations_hd)
    in
    List.iter
      (fun sl -> 
        let idxs = list_to_set (idx_of_smtlib sl) [] in
        let fvs = list_to_set (fvs_of_smtlib sl @ idxs) [] in
        if fvs = [] then
          (output_string oc "(assert ";
          (* smtlibの制約部分の記述 *)
            print_smtlib oc sl [] num; 
            output_string oc ( ")\n");)
        else
          (* (output_string oc "(assert (forall (";
          output_string oc (make_args fvs);
          output_string oc ") ";
          List.iter 
          (fun fv -> output_string oc (asprintf "(=> (<= -%d %s) (=> (<= %s %d) " (iter+1) fv fv (iter+1)))
          fvs;
          print_smtlib oc sl [] num; 
          List.iter 
          (fun _ -> output_string oc (asprintf "))" ))
          fvs;
          output_string oc ("))\n");)) *)
          let comb = generate_combinations fvs (range (-iter + 1) (iter+1)) in
          List.iter (fun map -> 
          (output_string oc "(assert ";
          (* smtlibの制約部分の記述 *)
            print_smtlib oc sl map num; 
            output_string oc ( ")\n");)
            )comb;
            output_string oc "\n";
            )          
      smtlibs
and print_smtlibs_sub oc unsat_core_flag num sl = 
  (* 制約内の重複を除いた自由変数のリスト *)
  let idxs = list_to_set (idx_of_smtlib sl) [] in
  let fvs = list_to_set ((fvs_of_smtlib sl)@idxs) [] in
  if unsat_core_flag then
    (if fvs = [] then
      (output_string oc "(assert (! ";
      print_smtlib oc sl [] num; 
      output_string oc (" :named sl" ^ (string_of_int !serial_num) ^ "))\n");
      serial_num := !serial_num + 1)
    else 
      (* smtlibの制約内に自由変数が存在する場合はfor allを挿入して制約を記述 *)
      (output_string oc "(assert (! (forall (";
      output_string oc (make_args fvs);
      output_string oc ") ";
      print_smtlib oc sl [] num; 
      output_string oc (") :named sl" ^ (string_of_int !serial_num) ^ "))\n");
      serial_num := !serial_num + 1))
  else 
    (if fvs = [] then
      (output_string oc "(assert ";
      (* smtlibの制約部分の記述 *)
      print_smtlib oc sl [] num; 
      output_string oc (")\n");)
    else 
      (* smtlibの制約内に自由変数が存在する場合はfor allを挿入して制約を記述 *)
      (output_string oc "(assert (forall (";
      output_string oc (make_args fvs);
      output_string oc ") ";
      print_smtlib oc sl [] num; 
      output_string oc ("))\n");))
  and make_args fvs = 
    match fvs with
    | [] -> ""
    | fv :: [] -> "(" ^ fv ^ " Int)" 
    | fv :: fvs' -> "(" ^ fv ^ " Int) " ^ (make_args fvs')
(* smtlib形式から自由変数のリストを返す関数 *)
and fvs_of_smtlib sl =
  match sl with 
  | Or (s1,s2) | And (s1,s2) | Imply (s1,s2) | Eq (s1,s2) | Lt (s1,s2) | Gt (s1,s2) 
  | Leq (s1,s2) | Geq (s1,s2) | Add (s1,s2) | Sub (s1,s2) | Mul (s1,s2) | Div (s1, s2) -> 
    (fvs_of_smtlib s1) @ (fvs_of_smtlib s2)
  | Not s -> 
    fvs_of_smtlib s
  (* | Div (s1,s2) -> 
    (fvs_of_smtlib s1) @ (fvs_of_smtlib s2) *)
  | FV fv -> 
    [fv]
  | IntPred (_,ids) | IntVarPred (_,_,ids) ->
    ids
  | PtrPred (_,_,s1,fvs, var_name) ->
    var_name :: (List.concat @@ List.map fvs_of_smtlib s1) @ fvs 
  | PtrVarPred (_,_,_,s1,fvs) ->
    "v" :: (List.concat @@ List.map fvs_of_smtlib s1) @ fvs
  | VarPred | True | Id _-> []
  | Ands ss ->
    List.concat (List.map fvs_of_smtlib ss)

(* 関数中の結果の表示 *)
let find_own_res fun_num id pos branch_trace z3res ty_env fvs =
  let simpleTy = lookup id ty_env in
  let depth = ref_depth simpleTy in
  (* Format.printf "%s : %a %d\n" id Syntax.pp_simpleTy simpleTy depth; *)
  let res = ref "" in
  (* 係数を探す *)
  let find_own_c l_or_h depth fvs =
    List.iter
      (fun fv ->
        let s = asprintf "c_%d_%s_%s_%s_%d%a_%d" fun_num l_or_h fv id pos pp_branch_trace branch_trace depth in
        let res_c = lookup s z3res in
        if res_c = Int Z.zero then () else 
        res := asprintf "%s + %a * %s" !res pp_value res_c fv;
          ) fvs
      in
      let fvs_ref = ref fvs in
  for depth' = depth downto 1 do
    let s1 = asprintf "o_%d_%s_%d%a_%d" fun_num id pos pp_branch_trace branch_trace depth' in
    let res1 = lookup s1 z3res in
    (* 所有権を表す下限の一次式の切片の宣言 *)
    let s2 = asprintf "d_%d__l_%s_%d%a_%d" fun_num id pos pp_branch_trace branch_trace depth' in
    let res2 = lookup s2 z3res in
    res := asprintf "%s/* %s : ref^%d [ %a" !res id depth' pp_value res2;
    find_own_c "_l" depth' !fvs_ref;
    (* 所有権を表す上限の一次式の切片の宣言 *)
    let s3 = asprintf "d_%d__h_%s_%d%a_%d" fun_num id pos pp_branch_trace branch_trace depth' in
    let res3 = lookup s3 z3res in
    res := asprintf "%s, %a" !res pp_value res3;
    find_own_c "_h" depth' !fvs_ref;
    (* 所有権を表す変数の宣言　o_(関数のシリアル番号)_(参照変数名)_(b(評価前) or e(評価後)) *)
    res := asprintf "%s] -> %a */\n" !res pp_value res1;
    let idx = asprintf "i_%d_%s_%dth" fun_num id depth in
    fvs_ref := idx::!fvs_ref
  done;
  !res

let rec print_idx oc fun_num all_cs =
  let (fun_name, _) = (List.nth all_cs fun_num) in
  let ty_env = lookup fun_name !all_tyenv in
  let rec print_fvs_sub (id, simpleTy) =
    let rec print_index id simpleTy =
      match simpleTy with
      | Syntax.SRef ty' -> 
        output_string oc 
        (sprintf "(declare-fun i_%d_%s_%dth () Int)\n" fun_num id (ref_depth simpleTy));
        print_index id ty'
      | _ -> () in
    match simpleTy with
    | Syntax.SRef _ -> print_index id simpleTy
    (* | Syntax.SInt ->
      output_string oc 
      (sprintf "(declare-fun %s () Int)\n" id);  *)
    | _ -> ()
  in
  List.iter print_fvs_sub ty_env

let printConstrRandInt oc fun_num all_cs =
  let (_, cs) = (List.nth all_cs fun_num) in
  let rec printConstrRandInt_sub cs =
    match cs with
    | [] -> ()
    | CLetUndet(id, cs) :: left->
      output_string oc 
        (sprintf "(declare-fun %s () Int)\n" id);
      printConstrRandInt_sub cs;
      printConstrRandInt_sub left
    | CIf(_, cs1, cs2, _) :: left -> 
      printConstrRandInt_sub cs1;
      printConstrRandInt_sub cs2;
      printConstrRandInt_sub left
    | _ :: left -> 
      printConstrRandInt_sub left in
  printConstrRandInt_sub cs


let print_cexample oc smtlibs z3_res =
  let rec print_cexample_sub smtlibs = 
  match smtlibs with
  | [] -> (output_string oc "(not true)";)
  | sl :: left ->
    let idxs = list_to_set (idx_of_smtlib sl) [] in
    let fvs = list_to_set ((fvs_of_smtlib sl)@idxs) [] in
    if fvs = [] then print_cexample_sub left
    else
      (output_string oc "(or (not ";
      print_smtlib' oc sl [] (-1) z3_res;
      output_string oc ")\n";
      print_cexample_sub left;
      output_string oc ")")
  in
  (output_string oc "(assert\n";
  print_cexample_sub smtlibs;
  output_string oc ")")


    (* ファイルに結果を出力 *)
let rec print_sat_ans oc varown_count fvs fun_num z3res all_cs =
  let (fun_name, constrs) = (List.nth all_cs fun_num) in
  let ty_env = lookup fun_name !all_tyenv in
  (* デバッグ用，環境変数の型を表示 *)
  (* let _ = List.map (fun x ->  Format.printf "%s : %a\n" (fst x) Syntax.pp_simpleTy (snd x)) ty_env in  *)
  let formatter = formatter_of_out_channel oc in
  let varown_count = List.rev varown_count in
  fprintf formatter "fun_name:%s \nownership\n" fun_name;
  (List.iter
    (fun (id,b_or_e,fun_num',depth) ->
      if fun_num' = fun_num then
        let fvs_ref = ref fvs in
        (for depth' = depth downto 1 do
          let s1 = sprintf "o_%d_%s_%s_%d" fun_num id b_or_e depth' in
          let res1 = lookup s1 z3res in
          (* 所有権を表す下限の一次式の切片の宣言 *)
          let s2 = sprintf "d_%d__l_%s_%s_%d" fun_num id b_or_e depth' in
          let res2 = lookup s2 z3res in
          fprintf formatter "%s %s : ref^%d [ %a" id b_or_e depth' pp_value res2;
          (* 下限の係数を宣言 *)
          print_declare_b_and_e_c formatter !fvs_ref "_l" id b_or_e fun_num z3res depth';
          (* 所有権を表す上限の一次式の切片の宣言 *)
          let s3 = sprintf "d_%d__h_%s_%s_%d" fun_num id b_or_e depth' in
          let res3 = lookup s3 z3res in
          fprintf formatter ", %a" pp_value res3;
          (* 上限の係数を宣言 *)
          print_declare_b_and_e_c formatter !fvs_ref "_h" id b_or_e fun_num z3res depth';
          (* 所有権を表す変数の宣言　o_(関数のシリアル番号)_(参照変数名)_(b(評価前) or e(評価後)) *)
          fprintf formatter "] -> %a\n" pp_value res1;
          let idx = asprintf "i_%d_%s_%dth" fun_num id depth in
          fvs_ref := idx :: !fvs_ref
        done)
      else 
        ()
      ) varown_count);
  let rec print_body_own branch_trace fvs cons =
    match cons with
    | CIf (_, c_lis1, c_lis2, _) ->
      let s1 = String.concat "" (List.map (print_body_own (Then :: branch_trace) fvs) c_lis1 ) in
      let s2 = String.concat "" (List.map (print_body_own (Else :: branch_trace) fvs) c_lis2) in
      asprintf "if exp then {\n  %s} else {\n%s}\n" s1 s2
    | CLetAddPtr (id1, id2, e, pos) ->
      let sl = exp_to_smtlib e in
      let s = cons_to_program cons in
      let res1 = find_own_res fun_num id1 pos branch_trace z3res ty_env fvs in
      let res2 = 
        if contains_element eq0_list id2 && sl = Id "1" then
          (remove_element eq0_list id2;
          find_own_res fun_num id2 pos branch_trace z3res ty_env fvs)
        else if contains_element eq0_list id2 
          then 
            let index = String.index (find_own_res fun_num id2 pos branch_trace z3res ty_env fvs) '\n' in
            let id2_outer = String.sub (find_own_res fun_num id2 pos branch_trace z3res ty_env fvs) 0 index in
            let id2_eq0 = find_own_res fun_num (id2^"_eq0") pos branch_trace z3res ty_env fvs in
            let id2_non0 = find_own_res fun_num (id2^"_non0") pos branch_trace z3res ty_env fvs in
            asprintf "%s\n%s%s" id2_outer id2_eq0 id2_non0
          else find_own_res fun_num id2 pos branch_trace z3res ty_env fvs in
      asprintf "%s%s%s" s res1 res2
    | CLetImmutAddPtr (id1, id2, _, pos) ->
      let s = cons_to_program cons in
      let res1 = find_own_res fun_num id1 pos branch_trace z3res ty_env fvs in
      let res2 = find_own_res fun_num id2 pos branch_trace z3res ty_env fvs in
      asprintf "%s%s%s" s res1 res2
    | CLetUndet(id, cs) ->
      let s1 = String.concat "" (List.map (print_body_own (branch_trace) (id::fvs)) cs ) in
      asprintf "let %s = _ in \n%s\n" id s1
    | CMkArray (id, _, _, pos) ->
      let s = cons_to_program cons in
      let res = find_own_res fun_num id pos branch_trace z3res ty_env fvs in
      asprintf "%s%s" s res
    | CAliasAddPtr (id1, id2, _, pos) ->
      let s = cons_to_program cons in
      let res1 = find_own_res fun_num id1 pos branch_trace z3res ty_env fvs in
      let res2 = find_own_res fun_num id2 pos branch_trace z3res ty_env fvs in
      asprintf "%s%s%s" s res1 res2
    | CAliasDeref (id1, id2, pos) ->
      remove_element eq0_list id2;
      let s = cons_to_program cons in
      let res1 = find_own_res fun_num id1 pos branch_trace z3res ty_env fvs in
      let res2 = find_own_res fun_num id2 pos branch_trace z3res ty_env fvs in
      asprintf "%s%s%s" s res1 res2
    | CAssignRef (id1, id2, pos) ->
      eq0_list := id1 :: !eq0_list;
      let s = cons_to_program cons in
      let res1_eq0 = find_own_res fun_num (id1^"_eq0") pos branch_trace z3res ty_env fvs in
      let res1_non0 = find_own_res fun_num (id1^"_non0") pos branch_trace z3res ty_env fvs in
      let res2 = find_own_res fun_num id2 pos branch_trace z3res ty_env fvs in
      asprintf "%s%s%s%s" s res1_eq0 res1_non0 res2
    | CAssignInt (_,_) ->
      let s = cons_to_program cons in
      asprintf "%s" s
    | CApp (_, args, pos) ->
      let s = cons_to_program cons in
      let print_arg_own arg = 
        match arg with
        | AId id -> 
          remove_element eq0_list id;
          let res = find_own_res fun_num id pos branch_trace z3res ty_env fvs in
          asprintf "%s" res
        | _ -> "" in
      let s1 = String.concat "" (List.map print_arg_own args) in
      asprintf "%s%s" s s1
    | CLetDeref (id1, id2, pos) ->
      eq0_list := id2 :: !eq0_list;
      let s = cons_to_program cons in
      let res1 = find_own_res fun_num id1 pos branch_trace z3res ty_env fvs in
      let res2_eq0 = find_own_res fun_num (id2^"_eq0") pos branch_trace z3res ty_env fvs in
      let res2_non0 = find_own_res fun_num (id2^"_non0") pos branch_trace z3res ty_env fvs in
      asprintf "%s%s%s%s" s res1 res2_eq0 res2_non0
    | _ -> cons_to_program cons in
  let prog = String.concat " " (List.map (print_body_own [] fvs) constrs) in
  fprintf formatter "%s" prog;
(* 関数評価前，評価後の上限下限の定数係数の宣言 *)
and print_declare_b_and_e_c formatter fvs l_or_h id b_or_e fun_num z3res depth =
  List.iter
    (fun fv ->
      let s = sprintf "c_%d_%s_%s_%s_%s_%d" fun_num l_or_h fv id b_or_e depth in
      let res = lookup s z3res in
      if res = Int Z.zero then () else 
      fprintf formatter " + %a * %s" pp_value res fv;
        ) fvs