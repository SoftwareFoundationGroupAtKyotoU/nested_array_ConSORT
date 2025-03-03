open Util
open Format
open SmtlibSyntax
open SimpleTyping
open Z3Syntax
open OwnConstraintSyntax

(* 代入，読み出しにより変則的な所有権の形をしているidのリスト *)
let eq0_list : id list ref = ref []

(* 文字列の末尾の一致判定 *)
let ends_with s suffix =
  let s_len = String.length s in
  let suffix_len = String.length suffix in
  if suffix_len > s_len then false
  else
    String.sub s (s_len - suffix_len) suffix_len = suffix

  (* 文字列の末尾の削除 *)
let remove_suffix s n =
  let s_len = String.length s in
  if n > s_len then s
  else
    String.sub s 0 (s_len - n)

(* 所有権計算に必要なsmtlibでの変数宣言 *)
let rec print_declare oc var_locations fvs fun_num =
  let formatter = formatter_of_out_channel oc in
  let rec nested_ref_declare id pos branch_trace depth fvs = 
    if depth < 1 then ()
    else
    (* 所有権を表す変数の宣言　o_(関数のシリアル番号)_(参照変数名)_(関数内での位置を表す整数)_(then or else)_depth *)
      (fprintf formatter "(declare-fun o_%d_%s_%d%a_%d () Real)\n" fun_num id pos pp_branch_trace branch_trace depth;
      (* 下限の係数を宣言 *)
      print_declare_c formatter fvs "l" id pos branch_trace fun_num depth;
      (* 所有権を表す下限の一次式の切片の宣言 
      [d + c1*x1 + ..., d' + c1'*x1 + ...] -> o のd
      d_(関数のシリアル番号)_l_(参照変数名)_(関数内での位置を表す整数)_(then or else)_depth *)
      fprintf formatter "(declare-fun d_%d_l_%s_%d%a_%d () Int)\n" fun_num id pos pp_branch_trace branch_trace depth;
      (* 上限の係数を宣言 *)
      print_declare_c formatter fvs "h" id pos branch_trace fun_num depth;
      (* 所有権を表す上限の一次式の切片の宣言 
      d_(関数のシリアル番号)_h_(参照変数名)_(関数内での位置を表す整数)_(then or else)_depth *)
      fprintf formatter "(declare-fun d_%d_h_%s_%d%a_%d () Int)\n" fun_num id pos pp_branch_trace branch_trace depth;
      (* 配列の添え字を表す変数の宣言
      i_(関数のシリアル番号)_(参照変数名)_(関数内での位置を表す整数)_(depth)th_(then or else) *)
      fprintf formatter "(declare-fun i_%d_%s_%d_%dth%a () Int)\n" fun_num id pos depth pp_branch_trace branch_trace;
      let idx = asprintf "i_%d_%s_%d_%dth%a" fun_num id pos depth pp_branch_trace branch_trace in
      let fvs' = idx::fvs in
      nested_ref_declare id pos branch_trace (depth-1) fvs') in
  (List.iter
    (fun (id,(pos,branch_trace, simpleTy)) ->
      let depth = ref_depth simpleTy in 
      nested_ref_declare id pos branch_trace depth fvs) var_locations);
(* 所有範囲の上限または下限の宣言 *)
and print_declare_c formatter fvs l_or_h id pos branch_trace fun_num depth =
  List.iter
    (fun fv ->
      (* smtlibに渡す参照の範囲の上限下限を表す一次式のうち，環境の自由変数の係数定数を宣言
      [d + c1*x1 + ..., d' + c1'*x1 + ...] -> o のc1やc1'
      c_(関数のシリアル番号)_(l(下限) or h(上限))_(自由変数名)_(参照変数名)_(関数内での位置を表す整数)_(if or el)_(参照の深さ) *)
      fprintf formatter "(declare-fun c_%d_%s_%s_%s_%d%a_%d () Int)\n"
       fun_num l_or_h fv id pos pp_branch_trace branch_trace depth) fvs

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
       print_declare_b_and_e_c formatter fvs "l" id b_or_e fun_num depth;
       (* 所有権を表す下限の一次式の切片の宣言 *)
       fprintf formatter "(declare-fun d_%d_l_%s_%s_%d () Int)\n" fun_num id b_or_e depth;
       (* 上限の係数を宣言 *)
       print_declare_b_and_e_c formatter fvs "h" id b_or_e fun_num depth;
       (* 所有権を表す上限の一次式の切片の宣言 *)
       fprintf formatter "(declare-fun d_%d_h_%s_%s_%d () Int)\n" fun_num id b_or_e depth;
       (* 配列の添え字を表す変数の宣言 *)
       fprintf formatter "(declare-fun i_%d_%s_%s_%dth () Int)\n" fun_num id b_or_e depth;
       let idx = asprintf "i_%d_%s_%s_%dth" fun_num id b_or_e depth in
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


(* 準smtlibの制約をちゃんとしたsmtlibの制約にしてファイルに書き出す関数 
  slはsmtlibの制約
mapは自由変数から整数への割り当て[(fv, -iter), (fv, -iter+1), ... (fv, iter)]
bool_idは使われていない？
numは篩型の識別番号？
*)
let rec print_smtlib oc sl bool_id map num = 
  match sl with 
  | Or (s1,s2) -> 
    (output_string oc "(or ";
     print_smtlib oc s1 bool_id map num;
     output_string oc " ";
     print_smtlib oc s2 bool_id map num;
     output_string oc ")")
  | And (s1,s2) -> 
    (output_string oc "(and ";
     print_smtlib oc s1 bool_id map num;
     output_string oc " ";
     print_smtlib oc s2 bool_id map num;
     output_string oc ")")
  | Imply (s1,s2) -> 
    (output_string oc "(=> ";
     print_smtlib oc s1 bool_id map num;
     output_string oc " ";
     print_smtlib oc s2 bool_id map num;
     output_string oc ")")
  | Not s -> 
    (output_string oc "(not ";
     print_smtlib oc s bool_id map num;
     output_string oc ")")
  | Eq (s1,s2) -> 
    (output_string oc "(= ";
     print_smtlib oc s1 bool_id map num;
     output_string oc " ";
     print_smtlib oc s2 bool_id map num;
     output_string oc ")")
  | Lt (s1,s2) -> 
    (output_string oc "(< ";
     print_smtlib oc s1 bool_id map num;
     output_string oc " ";
     print_smtlib oc s2 bool_id map num;
     output_string oc ")")
  | Gt (s1,s2) -> 
    (output_string oc "(> ";
     print_smtlib oc s1 bool_id map num;
     output_string oc " ";
     print_smtlib oc s2 bool_id map num;
     output_string oc ")")
  | Leq (s1,s2) -> 
    (output_string oc "(<= ";
     print_smtlib oc s1 bool_id map num;
     output_string oc " ";
     print_smtlib oc s2 bool_id map num;
     output_string oc ")")
  | Geq (s1,s2) -> 
    (output_string oc "(>= ";
     print_smtlib oc s1 bool_id map num;
     output_string oc " ";
     print_smtlib oc s2 bool_id map num;
     output_string oc ")")
  | Add (s1,s2) -> 
    (output_string oc "(+ ";
     print_smtlib oc s1 bool_id map num;
     output_string oc " ";
     print_smtlib oc s2 bool_id map num;
     output_string oc ")")
  | Sub (s1,s2) -> 
    (output_string oc "(- ";
     print_smtlib oc s1 bool_id map num;
     output_string oc " ";
     print_smtlib oc s2 bool_id map num;
     output_string oc ")")
  | Mul (s1,s2) -> 
    (output_string oc "(* ";
     print_smtlib oc s1 bool_id map num;
     output_string oc " ";
     print_smtlib oc s2 bool_id map num;
     output_string oc ")")
  (* | Div (s1,s2) -> 
    (output_string oc "(div ";
     print_smtlib oc s1 bool_id map num;
     output_string oc " ";
     print_smtlib oc s2 bool_id map num;
     output_string oc ")") *)
  | FV fv -> 
    (try
      let n = lookup fv map in
      output_string oc (string_of_int n)
    with Error _ -> output_string oc fv)
  | Id id -> output_string oc id
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
  | PtrPred (id,l,i_sl,ids) -> 
    (output_string oc ("(P" ^ (string_of_int num) ^ "_" ^ id ^ "_" ^ l ^ " ");
     print_smtlib oc i_sl bool_id map num;
     output_string oc " v";
     List.iter
       (fun id -> 
          output_string oc (" " ^ id)) ids;
     output_string oc ")")
  | PtrVarPred (num',id,be,i_sl,ids) -> 
    (output_string oc ("(P" ^ (string_of_int num') ^ "_" ^ id ^ "_" ^ be ^ " ");
     print_smtlib oc i_sl bool_id map num;
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
    | sl :: [] -> print_smtlib oc sl bool_id map num
    | _ -> 
      (output_string oc "(and";
       List.iter 
         (fun sl ->
            output_string oc " ";
            print_smtlib oc sl bool_id map num) smtlibs;
       output_string oc ")")

(* smtlibの制約をファイルに書き出し
oc 書き出し先
smtlibs 制約
is_unconcrete 自由変数を具体化するかどうか falseで具体化 trueで制約をそのまま書き出し
fvs 所有権termが依存できる自由変数
num 篩型用の数字
iter 自由変数を具体化する値の範囲 *)
let rec print_smtlibs oc smtlibs is_unconcrete fvs num iter =
  if is_unconcrete then
    List.iter (print_smtlibs_sub oc num) smtlibs
  else
    (* m :: m+1 :: ... :: n :: [] のリストを作成 *)
    let rec range m n =
      if m > n then []
      else m :: range (m + 1) n
    in
    (* 自由変数fv1, fv2, ... と整数リスト[m, m+1, ... , n]について 
    [[(fv1, m), (fv1, m+1), ... (fv1, n)], [(fv2, m), (fv2, m+1), ...]]
    を返す関数 
    fvの順番逆かも*)
    let rec generate_combinations fvs int_range =
      match fvs with
      | [] -> [[]]
      | hd :: tl ->
        let combinations_hd = List.map (fun x -> (hd, x)) int_range in
        let combinations_tl = generate_combinations tl int_range in
        List.flatten (List.map (fun a -> List.map (fun b -> a :: b) combinations_tl) combinations_hd)
    in
    (* [(fv1, -iter), (fv1, -iter+1), ... (fv1, iter), (fv2, -iter), (fv2, -iter+1), ...] 
    自由変数を-iterからiterに代入して所有権の値を計算するための準備*)
    let comb = generate_combinations fvs (range (-iter) iter) in
    (* assertにより制約をファイルに書き出す，自由変数に-iterからiterの数値の代入も行う *)
    List.iter
      (fun map ->
        (List.iter 
          (fun sl -> 
            output_string oc "(assert ";
            (* smtlibの制約部分の記述 
            slはsmtlibの制約
            mapは自由変数から整数への割り当て[(fv, -iter), (fv, -iter+1), ... (fv, iter)]
            numは特定のsmtlibの識別番号
            *)
            print_smtlib oc sl false map num; 
            output_string oc ")\n"
            ) smtlibs;
          output_string oc "\n")) comb
and print_smtlibs_sub oc num sl = 
  (* 制約内の重複を除いた自由変数のリスト *)
  let fvs = list_to_set (fvs_of_smtlib sl) [] in
  if fvs = [] then
    (output_string oc "(assert ";
    (* smtlibの制約部分の記述 *)
      print_smtlib oc sl true [] num; 
      output_string oc ")\n")
  else 
    (* smtlibの制約内に自由変数が存在する場合はfor allを挿入して制約を記述 *)
    (output_string oc "(assert (forall (";
      output_string oc (make_args fvs);
      output_string oc ") ";
      print_smtlib oc sl true [] num; 
      output_string oc "))\n")
and make_args fvs = 
  match fvs with
  | [] -> ""
  | fv :: [] -> "(" ^ fv ^ " Int)" 
  | fv :: fvs' -> "(" ^ fv ^ " Int) " ^ (make_args fvs')
(* smtlib形式から自由変数のリストを返す関数 *)
and fvs_of_smtlib sl =
  match sl with 
  | Or (s1,s2) -> 
    (fvs_of_smtlib s1) @ (fvs_of_smtlib s2)
  | And (s1,s2) -> 
    (fvs_of_smtlib s1) @ (fvs_of_smtlib s2)
  | Imply (s1,s2) -> 
    (fvs_of_smtlib s1) @ (fvs_of_smtlib s2)
  | Not s -> 
    fvs_of_smtlib s
  | Eq (s1,s2) -> 
    (fvs_of_smtlib s1) @ (fvs_of_smtlib s2)
  | Lt (s1,s2) -> 
    (fvs_of_smtlib s1) @ (fvs_of_smtlib s2)
  | Gt (s1,s2) -> 
    (fvs_of_smtlib s1) @ (fvs_of_smtlib s2)
  | Leq (s1,s2) -> 
    (fvs_of_smtlib s1) @ (fvs_of_smtlib s2)
  | Geq (s1,s2) -> 
    (fvs_of_smtlib s1) @ (fvs_of_smtlib s2)
  | Add (s1,s2) -> 
    (fvs_of_smtlib s1) @ (fvs_of_smtlib s2)
  | Sub (s1,s2) -> 
    (fvs_of_smtlib s1) @ (fvs_of_smtlib s2)
  | Mul (s1,s2) -> 
    (fvs_of_smtlib s1) @ (fvs_of_smtlib s2)
  (* | Div (s1,s2) -> 
    (fvs_of_smtlib s1) @ (fvs_of_smtlib s2) *)
  | FV fv -> 
    [fv]
  | Id _ -> 
    []
  | IntPred (_,ids) ->
    ids
  | IntVarPred (_,_,ids) ->
    ids
  | PtrPred (_,_,s1,fvs) ->
    "v" :: (fvs_of_smtlib s1) @ fvs 
  | PtrVarPred (_,_,_,s1,fvs) ->
    "v" :: (fvs_of_smtlib s1) @ fvs 
  | VarPred ->
    []
  | True ->
    []
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
        res := asprintf "%s + %a * %s" !res pp_value res_c fv;
          ) fvs
      in
      let fvs_ref = ref fvs in
  for depth' = depth downto 1 do
    let s1 = asprintf "o_%d_%s_%d%a_%d" fun_num id pos pp_branch_trace branch_trace depth' in
    let res1 = lookup s1 z3res in
    (* 所有権を表す下限の一次式の切片の宣言 *)
    let s2 = asprintf "d_%d_l_%s_%d%a_%d" fun_num id pos pp_branch_trace branch_trace depth' in
    let res2 = lookup s2 z3res in
    res := asprintf "%s/* %s : ref^%d [ %a" !res id depth' pp_value res2;
    find_own_c "l" depth' !fvs_ref;
    (* 所有権を表す上限の一次式の切片の宣言 *)
    let s3 = asprintf "d_%d_h_%s_%d%a_%d" fun_num id pos pp_branch_trace branch_trace depth' in
    let res3 = lookup s3 z3res in
    res := asprintf "%s, %a" !res pp_value res3;
    find_own_c "h" depth' !fvs_ref;
    (* 所有権を表す変数の宣言　o_(関数のシリアル番号)_(参照変数名)_(b(評価前) or e(評価後)) *)
    res := asprintf "%s] -> %a */\n" !res pp_value res1;
    let idx = asprintf "i_%d_%s_%d_%dth%a" fun_num id pos depth pp_branch_trace branch_trace in
    fvs_ref := idx::!fvs_ref
  done;
  !res


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
          let s2 = sprintf "d_%d_l_%s_%s_%d" fun_num id b_or_e depth' in
          let res2 = lookup s2 z3res in
          fprintf formatter "%s %s : ref^%d [ %a" id b_or_e depth' pp_value res2;
          (* 下限の係数を宣言 *)
          print_declare_b_and_e_c formatter !fvs_ref "l" id b_or_e fun_num z3res depth';
          (* 所有権を表す上限の一次式の切片の宣言 *)
          let s3 = sprintf "d_%d_h_%s_%s_%d" fun_num id b_or_e depth' in
          let res3 = lookup s3 z3res in
          fprintf formatter ", %a" pp_value res3;
          (* 上限の係数を宣言 *)
          print_declare_b_and_e_c formatter !fvs_ref "h" id b_or_e fun_num z3res depth';
          (* 所有権を表す変数の宣言　o_(関数のシリアル番号)_(参照変数名)_(b(評価前) or e(評価後)) *)
          fprintf formatter "] -> %a\n" pp_value res1;
          let idx = asprintf "i_%d_%s_%s_%dth" fun_num id b_or_e depth in
          fvs_ref := idx :: !fvs_ref
        done)
      else 
        ()
      ) varown_count);
  let rec print_body_own branch_trace cons =
    match cons with
    | CIf (_, c_lis1, c_lis2, _) ->
      let s1 = String.concat "" (List.map (print_body_own (Then :: branch_trace)) c_lis1 ) in
      let s2 = String.concat "" (List.map (print_body_own (Else :: branch_trace)) c_lis2) in
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
      let res1 = find_own_res fun_num (id1) pos branch_trace z3res ty_env fvs in
      let index = String.index res1 '\n' in
      let res1_outer = String.sub res1 0 index in
      let res1_eq0 = find_own_res fun_num (id1^"_eq0") pos branch_trace z3res ty_env fvs in
      let res1_non0 = find_own_res fun_num (id1^"_non0") pos branch_trace z3res ty_env fvs in
      let res2 = find_own_res fun_num id2 pos branch_trace z3res ty_env fvs in
      asprintf "%s%s\n%s%s%s" s res1_outer res1_eq0 res1_non0 res2
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
  let prog = String.concat " " (List.map (print_body_own []) constrs) in
  fprintf formatter "%s" prog;
(* 関数評価前，評価後の上限下限の定数係数の宣言 *)
and print_declare_b_and_e_c formatter fvs l_or_h id b_or_e fun_num z3res depth =
  List.iter
    (fun fv ->
      let s = sprintf "c_%d_%s_%s_%s_%s_%d" fun_num l_or_h fv id b_or_e depth in
      let res = lookup s z3res in
      fprintf formatter " + %a * %s" pp_value res fv;
        ) fvs