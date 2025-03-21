(* Module for ownership constraint generation(smtlib acceptable form) *)

open OwnConstraintSyntax
open SmtlibSyntax
open Util
open Format
open CollectOwnConstraint
open Syntax
open SimpleTyping

exception Unbound
exception ConstrError
(* exception Error of string *)

type var_locations_ty = (id * (tyvar * branch list * simpleTy)) list ref
(* 変数idとその変数の存在する場所 
(変数id, (変数の場所pos, branch_trace, simpleTy))のリスト*)
(* branch_traceはif文の分岐情報を表す *)
let var_locations : var_locations_ty = ref []

(* 所有権表現を表す組（上限下限，所有権の値）のリスト *)
let own_represents : own_represent list ref = ref []

(* 代入，読み出しにより変則的な所有権の形をしているidのリスト *)
let eq0_list : id list ref = ref []

(* 環境からidとbranch_traceに対応する関数内の位置posを返す *)
(* branch_traceはif文の分岐情報を表す *)
let rec lookup_pos id branch_trace env =
  match env with
  | [] -> 
    (* Format.printf "unbound %s %a\n" id pp_branch_trace branch_trace; *)
    raise Unbound
  | (x, (position, branch_trace', _)) :: left_env -> 
    (* Format.printf "%s: pos %d bt %a sty %a\n" x position pp_branch_trace branch_trace' pp_simpleTy simpleTy; *)
    if id = x && branch_trace = branch_trace' then position else lookup_pos id branch_trace left_env

(* 環境からidとbranch_traceに対応する関数内の直前の位置posを返す *)
let rec lookup_pre_pos id branch_trace env = 
  try
  match env with
  | [] -> 
    (* Format.printf "unbound pre %s %a\n" id pp_branch_trace branch_trace; *)
    raise Unbound
  | (x, _) :: left_env -> if id = x then lookup_pos id branch_trace left_env else lookup_pre_pos id branch_trace left_env
  with
  | Unbound ->
    (* Format.printf "unbound pre %s %a\n" id pp_branch_trace branch_trace; *)
    raise Unbound

(* var_locationsに新しい変数idを追加または既存のidの情報を更新 *)
let new_id id position branch_trace simpleTy =
  try
    let pos' = lookup_pos id branch_trace !var_locations in
    if pos' = position then ()
    else
      var_locations := (id, (position, branch_trace, simpleTy)) :: !var_locations
  with 
    Unbound -> var_locations := (id, (position, branch_trace, simpleTy)) :: !var_locations
    
let new_id' id position branch_trace own_represent =
  try
    let pos' = lookup_pos id branch_trace !var_locations in
    if pos' = position then ()
    else
      own_represents := own_represent :: !own_represents
  with 
    Unbound -> own_represents := own_represent :: !own_represents

(* resは返り値用のリスト，末尾再帰のため？
var_locations((変数id, (関数内での場所, branch_trace))のリスト)から
branch_traceの分岐が同じ変数idを重複なしでリストにして全て返す関数 *)
let rec collect_same_trace_vars branch_trace var_locations = 
  let rec iterative_collect_same_trace_vars branch_trace var_locations res =
    (match var_locations with
    | [] -> res
    | (x, (_,lst,_)) :: var_locations' -> 
      if List.mem x res || branch_trace <> lst then 
        iterative_collect_same_trace_vars branch_trace var_locations' res 
      else 
        iterative_collect_same_trace_vars branch_trace var_locations' (x :: res))
  in iterative_collect_same_trace_vars branch_trace var_locations []

(* id fun_num branch_traceを元にsmtlibに渡す所有権の変数を生成 *)
let make_own_var id fun_num branch_trace depth = 
  let var_pos = lookup_pos id branch_trace !var_locations in
  let var_name = asprintf "o_%d_%s_%d%a_%d" fun_num id var_pos pp_branch_trace branch_trace depth in 
  Id(var_name)
  (* Id("o_" ^ (string_of_int fun_num) ^ "_" ^ id ^ "_" ^ (string_of_int (lookup_pos id branch_trace !var_locations)) ^ (branch_trace_to_str branch_trace)) *)

(* id, fun_num branch_traceを元にsmtlibに渡す所有権の変数を生成 
      引数が表す場所での変数の直前の所有権の変数を表している*)
let make_pre_own_var id fun_num branch_trace depth = 
  let var_pre_pos = lookup_pre_pos id branch_trace !var_locations in
  let var_name = asprintf "o_%d_%s_%d%a_%d" fun_num id var_pre_pos pp_branch_trace branch_trace depth in 
  Id(var_name)

(* 関数評価の最初と最後の状態での所有権を表す
b_or_eはbまたはeでbeginとendの意 *)
let make_own_var_be id fun_num b_or_e depth = 
  let var_name = asprintf "o_%d_%s_%s_%d" fun_num id b_or_e depth in
  Id(var_name)

let rec make_bound_exp fvs id h_or_l fun_num branch_trace depth = 
  let id_pos = lookup_pos id branch_trace !var_locations in
  match fvs with
  | [] -> 
    let var_name = asprintf "d_%d_%s_%s_%d%a_%d" fun_num h_or_l id id_pos pp_branch_trace branch_trace depth in
    Id(var_name)
  | fv :: fvs' ->
    let var_name = asprintf "c_%d_%s_%s_%s_%d%a_%d" fun_num h_or_l fv id id_pos pp_branch_trace branch_trace depth in
    Add( Mul(Id(var_name), FV(fv)), make_bound_exp fvs' id h_or_l fun_num branch_trace depth)

(*直前の所有範囲の下限,または上限を環境変数の一次式で表す *)
let rec make_pre_bound_exp fvs id h_or_l fun_num branch_trace depth = 
  let id_pre_pos = lookup_pre_pos id branch_trace !var_locations in
  match fvs with
  | [] -> 
    let var_name = asprintf "d_%d_%s_%s_%d%a_%d" fun_num h_or_l id id_pre_pos pp_branch_trace branch_trace depth in
    Id(var_name)
  | fv :: fvs' ->
    let var_name = asprintf "c_%d_%s_%s_%s_%d%a_%d" fun_num h_or_l fv id id_pre_pos pp_branch_trace branch_trace depth in
    Add(Mul(Id(var_name), FV(fv)), make_pre_bound_exp fvs' id h_or_l fun_num branch_trace depth)

(* 所有範囲が等しいという制約を所有範囲の型の係数がそれぞれ等しいことで表した制約 *)
let rec same_bound_now_now fvs id1 id2 h_or_l fun_num branch_trace depth = 
  let id1_pos = lookup_pos id1 branch_trace !var_locations in
  let id2_pos = lookup_pos id2 branch_trace !var_locations in
  match fvs with
  | [] -> 
    let var_name1 = asprintf "d_%d_%s_%s_%d%a_%d" fun_num h_or_l id1 id1_pos pp_branch_trace branch_trace depth in
    let var_name2 = asprintf "d_%d_%s_%s_%d%a_%d" fun_num h_or_l id2 id2_pos pp_branch_trace branch_trace depth in
    Eq(Id var_name1, Id var_name2)
  | fv :: fvs' ->
    let var_name1 = asprintf "c_%d_%s_%s_%s_%d%a_%d" fun_num h_or_l fv id1 id1_pos pp_branch_trace branch_trace depth in
    let var_name2 = asprintf "c_%d_%s_%s_%s_%d%a_%d" fun_num h_or_l fv id2 id2_pos pp_branch_trace branch_trace depth in
    And(Eq(Id var_name1, Id var_name2), same_bound_now_now fvs' id1 id2 h_or_l fun_num branch_trace depth)

let rec same_bound_now_pre fvs id1 id2 h_or_l fun_num branch_trace depth = 
  let id1_pos = lookup_pos id1 branch_trace !var_locations in
  let id2_pos = lookup_pre_pos id2 branch_trace !var_locations in
  match fvs with
  | [] -> 
    let var_name1 = asprintf "d_%d_%s_%s_%d%a_%d" fun_num h_or_l id1 id1_pos pp_branch_trace branch_trace depth in
    let var_name2 = asprintf "d_%d_%s_%s_%d%a_%d" fun_num h_or_l id2 id2_pos pp_branch_trace branch_trace depth in
    Eq(Id var_name1, Id var_name2)
  | fv :: fvs' ->
    let var_name1 = asprintf "c_%d_%s_%s_%s_%d%a_%d" fun_num h_or_l fv id1 id1_pos pp_branch_trace branch_trace depth in
    let var_name2 = asprintf "c_%d_%s_%s_%s_%d%a_%d" fun_num h_or_l fv id2 id2_pos pp_branch_trace branch_trace depth in
    And(Eq(Id var_name1, Id var_name2), same_bound_now_now fvs' id1 id2 h_or_l fun_num branch_trace depth)

let rec same_bound_pre_pre fvs id1 id2 h_or_l fun_num branch_trace depth = 
  let id1_pos = lookup_pre_pos id1 branch_trace !var_locations in
  let id2_pos = lookup_pre_pos id2 branch_trace !var_locations in
  match fvs with
  | [] -> 
    let var_name1 = asprintf "d_%d_%s_%s_%d%a_%d" fun_num h_or_l id1 id1_pos pp_branch_trace branch_trace depth in
    let var_name2 = asprintf "d_%d_%s_%s_%d%a_%d" fun_num h_or_l id2 id2_pos pp_branch_trace branch_trace depth in
    Eq(Id var_name1, Id var_name2)
  | fv :: fvs' ->
    let var_name1 = asprintf "c_%d_%s_%s_%s_%d%a_%d" fun_num h_or_l fv id1 id1_pos pp_branch_trace branch_trace depth in
    let var_name2 = asprintf "c_%d_%s_%s_%s_%d%a_%d" fun_num h_or_l fv id2 id2_pos pp_branch_trace branch_trace depth in
    And(Eq(Id var_name1, Id var_name2), same_bound_now_now fvs' id1 id2 h_or_l fun_num branch_trace depth)

let rec same_bound_branch fvs id h_or_l fun_num branch1 branch2 depth = 
  let pos1 = lookup_pos id branch1 !var_locations in
  let pos2 = lookup_pos id branch2 !var_locations in
  match fvs with
  | [] -> 
    let var_name1 = asprintf "d_%d_%s_%s_%d%a_%d" fun_num h_or_l id pos1 pp_branch_trace branch1 depth in
    let var_name2 = asprintf "d_%d_%s_%s_%d%a_%d" fun_num h_or_l id pos2 pp_branch_trace branch2 depth in
    Eq(Id var_name1, Id var_name2)
  | fv :: fvs' ->
    let var_name1 = asprintf "c_%d_%s_%s_%s_%d%a_%d" fun_num h_or_l fv id pos1 pp_branch_trace branch1 depth in
    let var_name2 = asprintf "c_%d_%s_%s_%s_%d%a_%d" fun_num h_or_l fv id pos2 pp_branch_trace branch2 depth in
    And(Eq(Id var_name1, Id var_name2), same_bound_branch fvs' id h_or_l fun_num branch1 branch2 depth)

let make_idx_id fun_num id depth = 
  asprintf "i_%d_%s_%dth" fun_num id depth

(* 関数評価の最初と最後の状態での所有範囲の上限または下限を表す
b_or_eはbまたはeでbeginとendの意 *)
let rec make_bound_exp_be fvs id h_or_l fun_num b_or_e depth = 
  match fvs with
  | [] -> 
    let var_name = asprintf "d_%d_%s_%s_%s_%d" fun_num h_or_l id b_or_e depth in
    Id(var_name)
  | fv :: fvs' ->
    let var_name = asprintf "c_%d_%s_%s_%s_%s_%d" fun_num h_or_l fv id b_or_e depth in
    Add(Mul(Id(var_name), FV(fv)), make_bound_exp_be fvs' id h_or_l fun_num b_or_e depth)

let make_idx_bound_smtlib id idx fvs fun_num branch_trace depth =
  And(Leq(Id idx, make_bound_exp fvs id "h" fun_num branch_trace depth), 
    Geq(Id idx, make_bound_exp fvs id "l" fun_num branch_trace depth))

let make_idx_pre_bound_smtlib id idx fvs fun_num branch_trace depth =
  And(Leq(Id idx, make_pre_bound_exp fvs id "h" fun_num branch_trace depth), 
    Geq(Id idx, make_pre_bound_exp fvs id "l" fun_num branch_trace depth))

let make_idx_bound_smtlib_be id idx fvs fun_num b_or_e depth =
  And(Leq(Id idx, make_bound_exp_be fvs id "h" fun_num b_or_e depth), 
    Geq(Id idx, make_bound_exp_be fvs id "l" fun_num b_or_e depth))

(* smtlibで変数宣言するために必要そう？
変数のid, b or e, 関数の通し番号の組 *)
let varown_count = ref []

let make_if_smtlib id fvs fun_num branch_trace depth =
  let rec make_if_smtlib_sub fvs depth =
    if depth <= 0 then [], [], []
    else
      (* then節else節に分岐したときに元の所有権を引き継ぐ *)
      let sl_own, sl_then, sl_else = 
      [Eq(make_own_var id fun_num branch_trace depth, make_own_var id fun_num (Then :: branch_trace) depth);
        Eq(make_own_var id fun_num branch_trace depth, make_own_var id fun_num (Else :: branch_trace) depth);],
      [same_bound_branch fvs id "h" fun_num branch_trace (Then :: branch_trace) depth;
        same_bound_branch fvs id "l" fun_num branch_trace (Then :: branch_trace) depth;],
      [same_bound_branch fvs id "h" fun_num branch_trace (Else :: branch_trace) depth;
        same_bound_branch fvs id "l" fun_num branch_trace (Else :: branch_trace) depth;] in
      let idx = make_idx_id fun_num id depth in
      let fvs' = idx::fvs in
      (* 添え字が配列の境界内にあるという制約 *)
      let idx_bound fvs branch_trace x = 
        let idx = make_idx_id fun_num id depth in
        Imply(make_idx_bound_smtlib id idx fvs fun_num branch_trace depth, x) in
      let sl_own', sl_then', sl_else' =
        make_if_smtlib_sub fvs' (depth-1) in
      sl_own @ sl_own',
      sl_then @ List.map 
                (fun x -> idx_bound fvs branch_trace 
                  (idx_bound fvs (Then :: branch_trace) x)) sl_then',
      sl_else @ List.map
                (fun x -> idx_bound fvs branch_trace 
                  (idx_bound fvs (Else :: branch_trace) x)) sl_else'
  in 
  let sl_own, sl_then, sl_else = make_if_smtlib_sub fvs depth in
  sl_own @ sl_then @ sl_else

let make_post_if_smtlib fvs id fun_num branch_trace depth branch =
  let branch_trace' = branch :: branch_trace in
  let rec make_post_if_smtlib_sub fvs depth =
    if depth <= 0 then
      [], []
    else
      let idx = make_idx_id fun_num id depth in
      let fvs' = idx::fvs in
        (* 添え字が配列の境界内にあるという制約 *)
      let idx_bound fvs branch_trace = 
        make_idx_bound_smtlib id idx fvs fun_num branch_trace depth in
      let sl_own, sl_range = make_post_if_smtlib_sub fvs' (depth - 1) in
      [Leq(make_own_var id fun_num branch_trace depth, make_own_var id fun_num branch_trace' depth)]
        @ sl_own,
      [Geq(make_bound_exp fvs id "l" fun_num branch_trace depth, make_bound_exp fvs id "l" fun_num branch_trace' depth);
      Leq(make_bound_exp fvs id "h" fun_num branch_trace depth, make_bound_exp fvs id "h" fun_num branch_trace' depth);]
        @ (List.map
      (fun x -> Imply(And(idx_bound fvs branch_trace, idx_bound fvs branch_trace'),
        x))  sl_range) in
  let sl_own, sl_range = make_post_if_smtlib_sub fvs depth in
  List.map
  (fun x -> 
    Or(Eq(make_own_var id fun_num branch_trace depth, Id "0."), 
    Or(Gt(make_bound_exp fvs id "l" fun_num branch_trace depth,
      make_bound_exp fvs id "h" fun_num branch_trace depth), x)))
  (sl_own @ sl_range)

let make_mkarray_smtlib fvs id fun_num branch_trace depth upper_bound =
  let rec make_mkarray_sub depth =
    if depth <= 0 then []
    else 
      let sl = make_mkarray_sub (depth - 1) in
      let sl2 = [Eq(make_own_var id fun_num branch_trace depth, Id "0.")] in
      sl @ sl2 in
  let main_sl = [Eq(make_own_var id fun_num branch_trace depth, Id "1.");
    Eq(make_bound_exp fvs id "l" fun_num branch_trace depth, Id "0"); 
    Eq(make_bound_exp fvs id "h" fun_num branch_trace depth, upper_bound)] in 
  main_sl @ (make_mkarray_sub (depth - 1)) 

(* let id1 = id2(ref) + sl(int) in ...
完全な表現力は持っていない　所有範囲が分割→分割か共有→共有 *)
let make_letAddPtr_smtlib fvs fun_num branch_trace id1 id2 sl depth =
  (* 所有範囲を分割した場合 *)
  let idx_bound id idx fvs depth x = 
    Imply(make_idx_bound_smtlib id idx fvs fun_num branch_trace depth,
    x) in
  let idx_pre_bound id idx fvs depth x = 
    Imply(make_idx_pre_bound_smtlib id idx fvs fun_num branch_trace depth,
    x) in
  let rec make_letAppPtr_smtlib_div depth = 
    (* id2の0番目だけ特別扱いしており，ズレる幅が1ではないならばid2の先頭以外の所有権部分を変化 *)
    let id2 = if contains_element eq0_list id2 && sl <> Id "1" then id2^"_non0" else id2 in
    if depth <= 0 then True
    else
      let sl1 = And(Eq(make_pre_own_var id2 fun_num branch_trace depth, make_own_var id1 fun_num branch_trace depth),
      (Eq(make_pre_own_var id2 fun_num branch_trace depth, make_own_var id2 fun_num branch_trace depth))) in
      let sl2 = make_letAppPtr_smtlib_div (depth-1) in
      And(sl1, sl2) in
      (* 所有範囲が共有されている場合 *)
  let rec make_letAppPtr_smtlib_share depth =
    if contains_element eq0_list id2 && sl <> Id "1" then Not True else 
    (if depth <= 0 then True
    else
      let sl1 = Eq(make_pre_own_var id2 fun_num branch_trace depth,
      Add(make_own_var id1 fun_num branch_trace depth, make_own_var id2 fun_num branch_trace depth)) in
      let sl2 = make_letAppPtr_smtlib_share (depth-1) in
      And(sl1, sl2)) in
  (* 最上位が分割でも共有でも内側の所有範囲の処理は同じ
  新しくできたid1もid2も元のid2の所有範囲を引き継ぐ *)
  let rec make_letAppPtr_smtlib_range fvs1 fvs2 depth =
    if depth <= 0 then True, True
    else
      let idx1 = make_idx_id fun_num id1 depth in
      let fvs1' = idx1::fvs in
      let idx2 = make_idx_id fun_num id2 depth in
      let fvs2' = idx2::fvs in
      let sl1, sl2 = 
      And(Eq(make_pre_bound_exp fvs2 id2 "l" fun_num branch_trace depth,
            make_bound_exp fvs1 id1 "l" fun_num branch_trace depth),
          Eq(make_pre_bound_exp fvs2 id2 "h" fun_num branch_trace depth, 
            make_bound_exp fvs1 id1 "h" fun_num branch_trace depth)),
      And(Eq(make_bound_exp fvs1 id1 "l" fun_num branch_trace depth,
            make_bound_exp fvs2 id2 "l" fun_num branch_trace depth),
          Eq(make_bound_exp fvs1 id1 "h" fun_num branch_trace depth,
            make_bound_exp fvs2 id2 "h" fun_num branch_trace depth))  in
      let sl3, sl4 = make_letAppPtr_smtlib_range fvs1' fvs2' (depth-1) in
      And(sl1, 
        idx_pre_bound id2 idx2 fvs2 depth
          (idx_bound id1 idx1 fvs1 depth 
            (Imply(Eq(Id idx1, Id idx2), sl3)))),
      And(sl2, 
        idx_bound id1 idx1 fvs1 depth
          (idx_bound id2 idx2 fvs2 depth 
            (Imply(Eq(Id idx1, Id idx2),sl4)))) in
  (* 最上位の参照の所有範囲の分割の際の制約 *)
  let sl1 = 
      And(Eq(make_pre_own_var id2 fun_num branch_trace depth, make_own_var id1 fun_num branch_trace depth),
      And((Eq(make_pre_own_var id2 fun_num branch_trace depth, make_own_var id2 fun_num branch_trace depth),
      And(Eq(make_pre_bound_exp fvs id2 "l" fun_num branch_trace depth,
      make_bound_exp fvs id2 "l" fun_num branch_trace depth),
      And(Eq(make_bound_exp fvs id1 "l" fun_num branch_trace depth, Id "0"),
      And(Eq(make_bound_exp fvs id2 "h" fun_num branch_trace depth,
      Sub(sl, Id "1")),
      Eq(make_bound_exp fvs id1 "h" fun_num branch_trace depth,
      Sub(make_pre_bound_exp fvs id2 "h" fun_num branch_trace depth, sl)))))))) in
  (* 最上位の参照の所有範囲の共有の際の制約 *)
  let sl2 = 
    And(Eq(make_pre_own_var id2 fun_num branch_trace depth, 
      Add(make_own_var id1 fun_num branch_trace depth,
      make_own_var id2 fun_num branch_trace depth)),
    And(Eq(make_pre_bound_exp fvs id2 "l" fun_num branch_trace depth,
      Add(make_bound_exp fvs id1 "l" fun_num branch_trace depth, sl)),
    And(Eq(make_pre_bound_exp fvs id2 "l" fun_num branch_trace depth,
      make_bound_exp fvs id2 "l" fun_num branch_trace depth),
    And(Eq(make_pre_bound_exp fvs id2 "h" fun_num branch_trace depth, 
      Add(make_bound_exp fvs id1 "h" fun_num branch_trace depth, sl)),
    Eq(make_pre_bound_exp fvs id2 "h" fun_num branch_trace depth,
      make_bound_exp fvs id2 "h" fun_num branch_trace depth))))) in
  (* id1とid2のインデックスが入った自由変数の集合をそれぞれ構築 *)
  let idx1 = make_idx_id fun_num id1 depth in
  let fvs1 = idx1::fvs in
  let idx2 = make_idx_id fun_num id2 depth in
  let fvs2 = idx2::fvs in
  let sl3, sl4 = make_letAppPtr_smtlib_range fvs1 fvs2 (depth-1) in
  [And(sl1, make_letAppPtr_smtlib_div (depth-1));
    (* Or(And(sl1, make_letAppPtr_smtlib_div (depth-1)),
    And(sl2, make_letAppPtr_smtlib_share (depth-1))); *)
  idx_pre_bound id2 idx2 fvs depth
    (idx_bound id1 idx1 fvs depth
      (Imply(Eq(Id idx1, Add(Id idx2, sl)), sl3)));
  idx_bound id1 idx1 fvs depth
    (idx_bound id2 idx2 fvs depth
      (Imply(Eq(Id idx1, Add(Id idx2, sl)), sl4)));
  ]

(* let id1 = id2(ref) + 1 in ...
id2が最初の要素のみ所有権が異なりid1とid2が1だけズレた部分を指す場合 *)
let make_letAddPtr_smtlib_heuristic fvs fun_num branch_trace id1 id2 depth =
  let id2_0 = id2 ^ "_eq0" in
  let id2_non0 = id2 ^ "_non0" in
  (* 所有権の値の制約 *)
  let rec common depth = 
    if depth <= 0 then []
    else
      let sl1 = 
        [
          Eq(make_pre_own_var id2_non0 fun_num branch_trace depth, make_own_var id1 fun_num branch_trace depth);
          Eq(make_pre_own_var id2_0 fun_num branch_trace depth, make_own_var id2 fun_num branch_trace depth);
          Eq(make_own_var id2_0 fun_num branch_trace depth, Id "0.");
          Eq(make_own_var id2_non0 fun_num branch_trace depth, Id "0.");
        ] in
      let sl2 = common (depth-1) in
      sl1 @ sl2 in
  let rec same_range id2_0 id2 fvs2_0 fvs2 depth =
    if depth <= 0 then []
    else
      let idx2_0 = make_idx_id fun_num id2_0 depth in
      let fvs2'_0 = idx2_0::fvs2_0 in
      let idx2 = make_idx_id fun_num id2 depth in
      let fvs2' = idx2::fvs2 in
      let idx_bound id idx fvs = make_idx_bound_smtlib id idx fvs fun_num branch_trace depth in
      let idx_pre_bound id idx fvs = make_idx_pre_bound_smtlib id idx fvs fun_num branch_trace depth in
      let sl1 = 
        (* 添え字のたどり方が同じならば所有範囲は等しい *)
        [And(Eq(make_bound_exp fvs2 id2 "l" fun_num branch_trace depth,
          make_pre_bound_exp fvs2_0 id2_0 "l" fun_num branch_trace depth),
        Eq(make_bound_exp fvs2 id2 "h" fun_num branch_trace depth, 
          make_pre_bound_exp fvs2_0 id2_0 "h" fun_num branch_trace depth))] in
      let sl2 = same_range id2_0 id2 fvs2'_0 fvs2' (depth-1) in
      sl1 @
      List.map
      (fun x -> 
        Imply(idx_pre_bound id2_0 idx2_0 fvs2_0,
          Imply(idx_bound id2 idx2 fvs2, 
            Imply(Eq(Id idx2, Id idx2_0),
        x)))) sl2
        (* sl1 *)
      in
  (* 最上位の参照の所有範囲の分割の制約 *)
  let sl1 = 
      [
        Eq(make_bound_exp fvs id1 "l" fun_num branch_trace depth, Id "0");
        Eq(make_bound_exp fvs id1 "h" fun_num branch_trace depth,
          make_pre_bound_exp fvs id2_non0 "h" fun_num branch_trace depth);
        Eq(make_bound_exp fvs id2 "l" fun_num branch_trace depth, Id "0");
        Eq(make_bound_exp fvs id2 "h" fun_num branch_trace depth, Id "0");
      ] in
  let sl2 = common depth in
  try
    (* id1とid2のインデックスが入った自由変数の集合をそれぞれ構築 *)
  let idx1 = make_idx_id fun_num id1 depth in
  let fvs1 = idx1::fvs in
  let idx2 = make_idx_id fun_num id2 depth in
  let fvs2 = idx2::fvs in
  let idx2_0 = make_idx_id fun_num id2_0 depth in
  let fvs2_0 = idx2_0::fvs in
  let idx2_non0 = make_idx_id fun_num id2_non0 depth in
  let fvs2_non0 = idx2_non0::fvs in
  let idx_bound id idx fvs = make_idx_bound_smtlib id idx fvs fun_num branch_trace depth in
  sl1 @ sl2 
  @
  List.map
  (fun x ->
    Imply(Eq(Id idx2, Id "0"),
      Imply(Eq(Id idx2_0, Id "0"),
        Imply(idx_bound id2 idx2 fvs,
          Imply (idx_bound id2_0 idx2_0 fvs,
    x))))
    )
  (same_range id2_0 id2 fvs2_0 fvs2 (depth-1))
  @
  List.map
  (fun x ->
    Imply(idx_bound id2_non0 idx2_non0 fvs,
      Imply(idx_bound id1 idx1 fvs,
        Imply(Eq(Add(Id idx2_non0, Id "1"), Id idx1),
        x))))
  (same_range id2_non0 id1 fvs2_non0 fvs1 (depth-1))
  (* [
    Imply(And(idx_bound id2_non0 idx2_non0 fvs,
      And(idx_bound id1 idx1 fvs,
        Eq(Add(Id idx2_non0, Id "1"), Id idx1))),
    same_range id2_non0 id1 fvs2_non0 fvs1 (depth-1));
  ] *)
with | Unbound -> raise ConstrError

(* id1 := id2; ... *)
let make_assignRef_smtlib fvs fun_num branch_trace id1 id2 depth =
  let idx1 = make_idx_id fun_num id1 depth in
  let fvs1 = idx1::fvs in
  let id1_0 = id1 ^ "_eq0" in
  let idx1_0 = make_idx_id fun_num id1_0 depth in
  let fvs1_0 = idx1_0::fvs in
  let id1_non0 = id1 ^ "_non0" in
  let idx1_non0 = make_idx_id fun_num id1_non0 depth in
  let fvs1_non0 = idx1_non0::fvs in
  let idx_bound id idx fvs depth x = 
    Imply(make_idx_bound_smtlib id idx fvs fun_num branch_trace depth,
    x) in
  let idx_pre_bound id idx fvs depth x = 
    Imply(make_idx_pre_bound_smtlib id idx fvs fun_num branch_trace depth,
    x) in
  let rec common depth =
    if depth <= 0 then []
    else
      (* id2の所有権を全てid1に渡す
      id1をid1_0とid1_non0に分割しid1全体の所有権を表現する *)
      [Eq(make_own_var id2 fun_num branch_trace depth, Id "0.");
      Eq(make_pre_own_var id1 fun_num branch_trace depth, 
        make_own_var id1_non0 fun_num branch_trace depth);
      Eq(make_pre_own_var id2 fun_num branch_trace depth, 
        make_own_var id1_0 fun_num branch_trace depth);]
      @ (common (depth-1))
  in
  let rec first_element fvs1_0 fvs2 depth =
    let idx1_0 = make_idx_id fun_num id1_0 depth in
    let fvs1_0' = idx1_0::fvs1_0 in
    let idx2 = make_idx_id fun_num id2 depth in
    let fvs2' = idx2::fvs2 in
    if depth <= 0 then []
    else
      [
      Eq(make_pre_bound_exp fvs2 id2 "l" fun_num branch_trace depth, make_bound_exp fvs1_0 id1_0 "l" fun_num branch_trace depth);
      Eq(make_pre_bound_exp fvs2 id2 "h" fun_num branch_trace depth, make_bound_exp fvs1_0 id1_0 "h" fun_num branch_trace depth)]
      @ 
      List.map 
        (fun x -> 
          idx_pre_bound id2 idx2 fvs2 depth 
           (idx_bound id1_0 idx1_0 fvs1_0 depth 
            (Imply(Eq(Id idx1_0, Id idx2), x))))
        (first_element fvs1_0' fvs2' (depth-1))
  in 
  let rec not_first_element fvs1 fvs1_non0 depth =
    let idx1 = make_idx_id fun_num id1 depth in
    let fvs1' = idx1::fvs1 in
    let idx1_non0 = make_idx_id fun_num id1_non0 depth in
    let fvs1_non0' = idx1_non0::fvs1_non0 in
    if depth <= 0 then []
    else
      [Eq(make_pre_bound_exp fvs1 id1 "l" fun_num branch_trace depth, 
        make_bound_exp fvs1_non0 id1_non0 "l" fun_num branch_trace depth);
      Eq(make_pre_bound_exp fvs1 id1 "h" fun_num branch_trace depth, 
        make_bound_exp fvs1_non0 id1_non0 "h" fun_num branch_trace depth)]
      @ 
      List.map 
        (fun x -> 
          idx_bound id1_non0 idx1_non0 fvs1_non0 depth 
           (idx_pre_bound id1 idx1 fvs1 depth 
            (Imply(Eq(Id idx1, Id idx1_non0), x))))
          (not_first_element fvs1' fvs1_non0' (depth-1))
  in
  common (depth-1) 
  @ List.map
      (fun x -> Imply(Eq(Id idx1_0,Id  "0"), x)) 
      (first_element fvs1_0 fvs (depth-1)) 
  @ List.map
      (fun x -> 
        idx_pre_bound id1 idx1 fvs depth 
         (idx_bound id1_non0 idx1_non0 fvs depth 
          (Imply(And(Eq(Id idx1, Add(Id idx1_non0, Id "1")),
            Not(Eq(Id idx1,Id  "0"))), x))))
      (not_first_element fvs1 fvs1_non0 (depth-1))

let make_letDeref_smtlib fvs fun_num branch_trace id1 id2 depth =
  let idx2 = make_idx_id fun_num id2 depth in
  let fvs2 = idx2::fvs in
  let id2_0 = id2 ^ "_eq0" in
  let id2_non0 = id2 ^ "_non0" in
  let idx2_non0 = make_idx_id fun_num id2_non0 depth in
  let fvs2_non0 = idx2_non0::fvs in
  let idx_bound id idx fvs depth x = 
    Imply(make_idx_bound_smtlib id idx fvs fun_num branch_trace depth,
    x) in
  let idx_pre_bound id idx fvs depth x = 
    Imply(make_idx_pre_bound_smtlib id idx fvs fun_num branch_trace depth,
    x) in
  let rec common depth =
    if depth <= 0 then []
    else
      (* id2の先頭の所有権を全てid1に渡す
      id2をid2_0とid2_non0に分割しid2全体の所有権を表現する *)
      [Eq(make_pre_own_var id2 fun_num branch_trace depth, make_own_var id1 fun_num branch_trace depth);
      Eq(make_pre_own_var id2 fun_num branch_trace depth, make_own_var id2_non0 fun_num branch_trace depth);
      Eq(make_own_var id2_0 fun_num branch_trace depth, Id "0")]
      @ (common (depth-1))
  in
  let rec make_letDeref_smtlib_same_range id1 id2 fvs1 fvs2 depth =
    if depth <= 0 then []
    else 
      let idx1 = make_idx_id fun_num id1 depth in
      let fvs1' = idx1::fvs1 in
      let idx2 = make_idx_id fun_num id2 depth in
      let fvs2' = idx2::fvs2 in
      let sl1 = 
        [
        Eq(make_pre_bound_exp fvs2 id2 "l" fun_num branch_trace depth, make_bound_exp fvs1 id1 "l" fun_num branch_trace depth);
        Eq(make_pre_bound_exp fvs2 id2 "h" fun_num branch_trace depth, make_bound_exp fvs1 id1 "h" fun_num branch_trace depth);
        ] in
      let sl2 = make_letDeref_smtlib_same_range id1 id2 fvs1' fvs2' (depth-1) in
      sl1 @ 
      List.map 
      (fun x -> idx_bound id1 idx1 fvs1 depth 
        (idx_pre_bound id2 idx2 fvs2 depth 
          (Imply(Eq(Id idx1, Id idx2), x))))
      sl2 in
  common depth @
  (* 最も外側の添え字が0かそうでないかで場合分け *)
  List.map (fun x -> Imply(Eq(Id idx2, Id "0"), x)) (make_letDeref_smtlib_same_range id1 id2 fvs fvs2 (depth-1)) @
  List.map 
  (fun x -> 
    idx_pre_bound id2 idx2 fvs depth
     (idx_bound id2_non0 idx2_non0 fvs depth
       (Imply(And(Not(Eq(Id idx2, Id "0")),
         Eq(Id idx2, Add(Id idx2_non0, Id "1"))), x)))) 
         (make_letDeref_smtlib_same_range id2_non0 id2 fvs2_non0 fvs2 (depth-1))

(* alias(id1 = *id2) *)
let make_aliasDeref_smtlib fvs fun_num branch_trace id1 id2 depth =
  let id2_non0 = id2 ^ "_non0" in
  let idx_bound id idx fvs depth x = 
    Imply(make_idx_bound_smtlib id idx fvs fun_num branch_trace depth,
    x) in
  let idx_pre_bound id idx fvs depth x = 
    Imply(make_idx_pre_bound_smtlib id idx fvs fun_num branch_trace depth,
    x) in
  let rec common depth =
    if depth <= 0 then []
    else
      [
      Imply(Leq(make_own_var id2_non0 fun_num branch_trace depth, make_pre_own_var id1 fun_num branch_trace depth),
        Eq(make_own_var id2 fun_num branch_trace depth, make_own_var id2_non0 fun_num branch_trace depth));
      Imply(Lt(make_pre_own_var id1 fun_num branch_trace depth, make_own_var id2_non0 fun_num branch_trace depth),
        Eq(make_own_var id2 fun_num branch_trace depth, make_pre_own_var id1 fun_num branch_trace depth));
      Eq(make_own_var id1 fun_num branch_trace depth, Id "0.");
      ]
      @ (common (depth-1)) in
  let rec make_aliasDeref_smtlib_same_range_pre fvs1 fvs2_non0 depth =
    if depth <= 0 then []
    else 
      let idx1 = make_idx_id fun_num id1 depth in
      let fvs1' = idx1::fvs1 in
      let idx2_non0 = make_idx_id fun_num id2_non0 depth in
      let fvs2'_non0 = idx2_non0::fvs2_non0 in
      let sl1 = 
        [
          Eq(make_bound_exp fvs2_non0 id2_non0 "l" fun_num branch_trace depth, make_pre_bound_exp fvs1 id1 "l" fun_num branch_trace depth);
          Eq(make_bound_exp fvs2_non0 id2_non0 "h" fun_num branch_trace depth, make_pre_bound_exp fvs1 id1 "h" fun_num branch_trace depth);
        ] in
      let sl2 = make_aliasDeref_smtlib_same_range_pre fvs1' fvs2'_non0 (depth-1) in
      sl1 @ 
      List.map
        (fun x -> 
          idx_pre_bound id1 idx1 fvs1 depth 
          (idx_bound id2_non0 idx2_non0 fvs2_non0 depth 
            (Imply(Eq(Id idx1, Id idx2_non0), x)))) 
      sl2 in
  let rec make_aliasDeref_smtlib_same_range_post fvs2 fvs2_non0 depth =
    if depth <= 0 then []
    else 
      let idx2 = make_idx_id fun_num id2 depth in
      let fvs2' = idx2::fvs2 in
      let idx2_non0 = make_idx_id fun_num id2_non0 depth in
      let fvs2'_non0 = idx2_non0::fvs2_non0 in
      let sl1 = 
        [
        Eq(make_bound_exp fvs id2_non0 "l" fun_num branch_trace depth, make_bound_exp fvs id2 "l" fun_num branch_trace depth);
        Eq(make_bound_exp fvs id2_non0 "h" fun_num branch_trace depth, make_bound_exp fvs id2 "h" fun_num branch_trace depth);
        ] in
      let sl2 = make_aliasDeref_smtlib_same_range_post fvs2' fvs2'_non0 (depth-1) in
      sl1 @ 
      List.map
        (fun x -> 
          idx_bound id2 idx2 fvs2 depth 
            (idx_bound id2_non0 idx2_non0 fvs2_non0 depth 
              (Imply(Eq(Id idx2, Id idx2_non0), x))))
      sl2 in
  let idx2 = make_idx_id fun_num id2 depth in
  let fvs2 = idx2::fvs in
  let idx2_non0 = make_idx_id fun_num id2_non0 depth in
  let fvs2_non0 = idx2_non0::fvs in
  common (depth-1) 
  @ (List.map 
    (fun x -> Imply(Eq(Sub(Id "0", Id "1"), Id idx2_non0), x)) 
    (make_aliasDeref_smtlib_same_range_pre fvs fvs2_non0 (depth-1)))
  @ (List.map 
    (fun x -> Imply(Eq(Id idx2, Add(Id idx2_non0, Id "1")),
      Imply(make_idx_bound_smtlib id2 idx2 fvs fun_num branch_trace depth, x))) 
    (make_aliasDeref_smtlib_same_range_post fvs2 fvs2_non0 (depth-1)))
    

let p c =
  match c with
  | CIf (exp, _, _, pos) -> print_exp exp;print_string "cif ";print_int pos;print_string "\n"; flush stdout;
  | CLetDeref (_ , _ , pos) -> print_string "cletderef ";print_int pos;print_string "\n"; flush stdout;
  | CLetAddPtr (_ , _ , _ , pos) -> print_string "cletaddptr ";print_int pos;print_string "\n"; flush stdout;
  | CMkArray (_ , _ , _ , pos) -> print_string "cmkarray ";print_int pos;print_string "\n"; flush stdout;
  | CAssignInt (_ , pos) -> print_string "cassiginint ";print_int pos;print_string "\n"; flush stdout;
  | CAssignRef (_ , _ , pos) -> print_string "cassignref ";print_int pos;print_string "\n"; flush stdout;
  | CAliasDeref (_ , _ , pos) -> print_string "caliasderef ";print_int pos;print_string "\n"; flush stdout;
  | CAliasAddPtr (id , id2 , _ , _) -> print_string ("caliasaddptr " ^ id ^ " " ^ id2 ^ "\n"); flush stdout;
  | CDeref (_ , pos) -> print_string "cderef ";print_int pos;print_string "\n"; flush stdout;
  | CApp (_ ,_ , pos) -> print_string "capp ";print_int pos;print_string "\n"; flush stdout;
  | _ -> print_string "other\n"; flush stdout;()
(** Main procedure for generating the ownership constraints 
オーナーシップ制約生成のためのメイン手続き
fvs: 関数引数のうちint型である変数の名前
fun_num: 関数の通し番号
funnames_numberings: 関数名と通し番号の組のリスト いらんかも？？？
branch_trace: if節のどちらを通ってきたかを表す文字列のリスト
c: 制約のリスト
関数内の所有権は関数の整数引数とインデックスにしか依存できない？
*)
let rec constr_to_smtlib fvs fun_num funnames_numberings branch_trace ty_env c =
  (* p c; flush stdout; *)
  match c with
  | CIf (e,cs1,cs2,pos) -> 
    (* if式直前の変数idのリスト *)
    let ids_pre = collect_same_trace_vars branch_trace !var_locations in 
    let eq0_ids_pre = !eq0_list in
    (* var_locations((変数id, (所有権のレベルl, branch_trace))のリスト)にthenブランチとelseブランチに対応する変数idが追加 *)
    List.iter (fun id -> 
      let simpleTy = lookup id ty_env in
      new_id id pos (Then :: branch_trace) simpleTy; 
      new_id id pos (Else :: branch_trace) simpleTy) ids_pre;
    let constraints_pre = 
      (* 制約のリスト[Eq(...); Eq(...); ..., Eq(...)]を作る *)
      List.concat (List.map 
        (fun id -> 
          let depth = ref_depth (lookup id ty_env) in
          (* if式の制約
           if直前，then節に入った時，else節に入った時の所有権は等しい
           if直前，then節に入った時，else節に入った時の所有範囲の下限は等しい
           if直前，then節に入った時，else節に入った時の所有範囲の上限は等しい*)
           make_if_smtlib id fvs fun_num branch_trace depth
        ) ids_pre) in
    (* then節側の制約をsmtlibが読める制約の形に直す *)
    let constraints1 = List.concat (List.map (constr_to_smtlib fvs fun_num funnames_numberings (Then :: branch_trace) ty_env) cs1) in
    (* 条件式が成り立つならばthen節の制約が成り立つ，という形に変更 *)
    let constraints1' = List.map (fun s -> Imply(exp_to_smtlib e, s)) constraints1 in
    (* else節側の制約をsmtlibが読める制約の形に直す *)
    let constraints2 = List.concat (List.map (constr_to_smtlib fvs fun_num funnames_numberings (Else :: branch_trace) ty_env) cs2) in
    (* 条件式が成り立たないならばelse節の制約が成り立つ，という形に変更 *)
    let constraints2' = List.map (fun s -> Imply(Not(exp_to_smtlib e), s)) constraints2 in
    (* then節評価後の変数のリスト *)
    let ids_post_if = collect_same_trace_vars (Then :: branch_trace) !var_locations in
    (* else節評価後の変数のリスト *)
    eq0_list := eq0_ids_pre;
    (* ここで所有権の形の統合 *)
    let ids_post_el = collect_same_trace_vars (Else :: branch_trace) !var_locations in
    (* then節とelse節評価後の変数のリストを結合 *)
    let ids_post = union_list ids_post_if ids_post_el in
    (* let _ = (List.map (fun x -> Format.printf "%s:%a\n" (fst x) pp_simpleTy (snd x)) ty_env) in *)
    (* then節とelse節評価後の各変数について，レベルを1増やしてvar_locationsに追加 *)
    List.iter (fun id -> 
      (* Format.printf "5\n"; flush stdout;  *)
      let simpleTy = lookup id ty_env in
      new_id id (pos+1) branch_trace simpleTy) ids_post;
    let constraints_post_if = 
      List.map 
        (fun s -> Imply(exp_to_smtlib e, s))
        (List.concat (List.map 
        (* then式評価後の各変数について 
            条件式がなりたつならば
          　　評価前の所有権が0と等しい　または
                (評価後の所有権はthen節評価時の所有権以下　かつ
                評価後の所有範囲の下限はthen節評価時の所有範囲の下限以上　かつ
                評価後の所有範囲の上限はthen節評価時の所有範囲の上限以下) *)
          (fun id -> 
            let depth = ref_depth (lookup id ty_env) in
            make_post_if_smtlib fvs id fun_num branch_trace depth Then
          ) ids_post_if)) in
    let constraints_post_el = 
      List.map 
        (fun s -> Imply(Not(exp_to_smtlib e), s))
        (* else式評価後の各変数について 
            条件式がなりたたないならば
          　　評価前の所有権が0と等しい　または
                (評価後の所有権はelse節評価時の所有権以下　かつ
                評価後の所有範囲の下限はelse節評価時の所有範囲の下限以上　かつ
                評価後の所有範囲の上限はelse節評価時の所有範囲の上限以下) *)
        (List.concat (List.map 
          (fun id -> 
            let depth = ref_depth (lookup id ty_env) in
            make_post_if_smtlib fvs id fun_num branch_trace depth Else
          ) ids_post_el)) in
    eq0_list := eq0_ids_pre;
    (* ここで所有権の形の統合 *)
    (* 制約をつなげて返す *)
    constraints_pre @ constraints1' @ constraints2' @ 
    constraints_post_if @ constraints_post_el
  (* | CLet (id1,id2,l) -> (* let x = y in ... *) 
    (* x,yに対応するvar_locationsを追加
    ここ下とマージできる *)
    new_id id1 l branch_trace; new_id id2 l branch_trace;
    (* 
    評価直前のyの所有権は評価後のxとyの所有権の和以上　または
    評価後のxの所有範囲の下限は評価後のyの所有範囲の上限より大きい　または
    評価後のyの所有範囲の上限は評価後のxの所有範囲の下限より大きい;
    評価直前のyの所有権は評価後のxの所有権以上;
    評価直前のyの所有権は評価後のyの所有権以上;
    評価直前のyの所有範囲の下限は評価後のxの所有範囲の下限以下;
    評価直前のyの所有範囲の下限は評価後のyの所有範囲の下限以下;
    評価直前のyの所有範囲の上限は評価後のxの所有範囲の上限以上;
    評価直前のyの所有範囲の上限は評価後のyの所有範囲の上限以上;
    評価後のxの添え字の下限は評価後のxの添え字の上限以下;
    評価後のyの添え字の下限は評価後のyの添え字の上限以下*)
    [Or(Geq(id2_pre_own, Add(id1_post_own, id2_post_own)),
     Or(Gt(make_bound_exp fvs id1 fun_num branch_trace, make_bound_exp fvs id2 fun_num branch_trace), 
        Gt(make_bound_exp fvs id2 fun_num branch_trace, make_bound_exp fvs id1 fun_num branch_trace)));
     Geq(id2_pre_own, id1_post_own);
     Geq(id2_pre_own, id2_post_own);
     Leq(id2_pre_scope_low, make_bound_exp fvs id1 fun_num branch_trace);
     Leq(id2_pre_scope_low, make_bound_exp fvs id2 fun_num branch_trace);
     Geq(id2_pre_scope_high, make_bound_exp fvs id1 fun_num branch_trace);
     Geq(id2_pre_scope_high, make_bound_exp fvs id2 fun_num branch_trace);
     Leq(make_bound_exp fvs id1 fun_num branch_trace, make_bound_exp fvs id1 fun_num branch_trace);
     Leq(make_bound_exp fvs id2 fun_num branch_trace, make_bound_exp fvs id2 fun_num branch_trace)]  *)
  | CLetAddPtr (id1,id2,e,pos) ->  (* Corresponds to the example on p.15, let x = y + num in ... *)
    (* x,yに対応するvar_locationsを追加 *)
    let simpleTy = lookup id2 ty_env in
    new_id id1 pos branch_trace simpleTy; 
    new_id id2 pos branch_trace simpleTy;
    (* numをsmtlibの読める形に変形 *)
    let sl1 = exp_to_smtlib e in
    let depth = ref_depth (lookup id2 ty_env) in
    (* 
    評価直前のyの所有権は評価後のxとyの所有権の和以上　または
    評価後のxの所有範囲の下限+numは評価後のyの所有範囲の上限より大きい　または
    評価後のyの所有範囲のは評価後のxの所有範囲の添え字の下限+numより大きい;
    評価直前のyの所有権は評価後のxの所有権以上;
    評価直前のyの所有権は評価後のyの所有権以上;
    評価直前のyの所有範囲の下限は評価後のxの所有範囲の下限+num以下;
    評価直前のyの所有範囲の下限は評価後のyの添え字の下限以下;
    評価直前のyの所有範囲の上限は評価後のxの所有範囲の添え字の上限+num以上;
    評価直前のyの所有範囲の上限は評価後のyの添え字の上限以上 *)
    let sl2 = 
      if contains_element eq0_list id2 && sl1 <> Id "1" then 
        (* (new_id (id2^"_eq0") pos branch_trace (SRef SInt);
        new_id (id2^"_non0") pos branch_trace (SRef SInt);
        same_own (id2^"_eq0") fvs fun_num branch_trace (depth-1))  *)
        []
      else [] in
    if contains_element eq0_list id2 && sl1 = Id "1" then
      (remove_element eq0_list id2;
      new_id (id2^"_eq0") pos branch_trace simpleTy;
      new_id (id2^"_non0") pos branch_trace simpleTy;
      make_letAddPtr_smtlib_heuristic fvs fun_num branch_trace id1 id2 depth)
    else
      sl2 @ make_letAddPtr_smtlib fvs fun_num branch_trace id1 id2 sl1 depth  
  | CMkArray (id,e, simpleTy,pos) -> 
    let upper_bound =
      match e with
      | ILit i -> Id (string_of_int (i-1))
      | Var id -> FV(id)
      | _ -> raise ConstrError in
    (* let x = alloc i in ... *)
    (* xに対応するvar_locationsを追加 *)
    new_id id pos branch_trace simpleTy;
    (* 
    xの所有権は1;
    xの所有範囲の下限は0;
    xの所有範囲の上限はe-1 *)
    let depth = ref_depth (lookup id ty_env) in
    make_mkarray_smtlib fvs id fun_num branch_trace depth upper_bound
  | CAssignInt (id,_) -> 
    (* x := num; ... *)
    (* 
    xの所有権は1;
    xの所有範囲の下限は0以下;
    xの所有範囲の上限は0以上 *)
    [Eq(make_own_var id fun_num branch_trace 1, Id "1");
     Leq(make_bound_exp fvs id "l" fun_num branch_trace 1, Id "0"); 
     Geq(make_bound_exp fvs id "h" fun_num branch_trace 1, Id "0")]
  | CAssignRef (id1, id2, pos) -> 
    (* id1 := id2; ... *)
    let id1_eq0 = id1 ^ "_eq0" in
    let id1_non0 = id1 ^ "_non0" in
    eq0_list := id1 :: !eq0_list;
    let id1_simpleTy = lookup id1 ty_env in
    let id2_simpleTy = lookup id2 ty_env in
    new_id id1 pos branch_trace id1_simpleTy; 
    new_id id1_eq0 pos branch_trace id1_simpleTy; 
    new_id id1_non0 pos branch_trace id1_simpleTy;
    new_id id2 pos branch_trace id2_simpleTy; 
    let id1_depth = ref_depth id1_simpleTy in
    (* 所有権が1であり，代入される側の一番外側の所有権，所有範囲は変化しない *)
    (* id1の所有範囲の下限は0と仮定 *)
    let sl1 = [Eq(make_pre_own_var id1 fun_num branch_trace id1_depth, Id "1.");
     Eq(make_pre_bound_exp fvs id1 "l" fun_num branch_trace id1_depth, Id "0"); 
     Geq(make_pre_bound_exp fvs id1 "h" fun_num branch_trace id1_depth, Id "0");
     Eq(make_pre_own_var id1 fun_num branch_trace id1_depth, make_own_var id1_eq0 fun_num branch_trace id1_depth);
     Eq(make_pre_own_var id1 fun_num branch_trace id1_depth, make_own_var id1_non0 fun_num branch_trace id1_depth);
     Eq(make_bound_exp fvs id1_eq0 "l" fun_num branch_trace id1_depth, Id "0");
     Eq(make_bound_exp fvs id1_eq0 "h" fun_num branch_trace id1_depth, Id "0");
     Eq(make_bound_exp fvs id1_non0 "l" fun_num branch_trace id1_depth, Id "0");
     Eq(make_bound_exp fvs id1_non0 "h" fun_num branch_trace id1_depth, 
      Sub(make_pre_bound_exp fvs id1 "h" fun_num branch_trace id1_depth, Id"1"))] in
    let sl2 = make_assignRef_smtlib fvs fun_num branch_trace id1 id2 id1_depth in
    sl1 @ sl2
  | CAliasAddPtr (id1,id2,e,pos) -> (* alias(id1 = id2 + num); ... *)
     (* x,yに対応するvar_locationsを追加 *)
    let simpleTy = lookup id2 ty_env in
    new_id id1 pos branch_trace simpleTy; 
    new_id id2 pos branch_trace simpleTy;
    let depth = ref_depth simpleTy in
      (* numをsmtlibの制約の形に変形 *)
    let sl = exp_to_smtlib e in
    (* 添え字が所有権範囲内にあるという制約 *)
    let idx_bound id idx fvs depth x = 
      Imply(make_idx_bound_smtlib id idx fvs fun_num branch_trace depth,
      x) in
    let idx_pre_bound id idx fvs depth x = 
      Imply(make_idx_pre_bound_smtlib id idx fvs fun_num branch_trace depth,
      x) in
    (* id1の所有権をid2に集約 *)
    let make_aliasdAddPtr_smtlib_gather depth =
      let rec common depth =
        if depth <= 0 then True
        else
          (* id1とid2の小さい方の所有権を採用 *)
          let sl1 = 
              And(Imply(Leq(make_pre_own_var id1 fun_num branch_trace depth,
                make_pre_own_var id2 fun_num branch_trace depth),
              Eq(make_pre_own_var id1 fun_num branch_trace depth,
                make_own_var id2 fun_num branch_trace depth)),
              Imply(Leq(make_pre_own_var id2 fun_num branch_trace depth,
                make_pre_own_var id1 fun_num branch_trace depth),
              Eq(make_pre_own_var id2 fun_num branch_trace depth,
                make_own_var id2 fun_num branch_trace depth))) in
          (* id1の所有権は0になる *)
          let sl2 = 
            Eq(make_own_var id1 fun_num branch_trace depth, Id "0") in
          (* 再帰処理 *)
          let sl3 = common (depth-1) in
          And(sl1, And(sl2, sl3)) in
      let rec same_range_pre fvs1 fvs2 depth =
        if depth <= 0 then True
        else
          let idx1 = make_idx_id fun_num id1 depth in
          let fvs1' = idx1::fvs1 in
          let idx2 = make_idx_id fun_num id2 depth in
          let fvs2' = idx2::fvs2 in
          (* id1とid2の元の所有範囲は等しい *)
          let sl1 = 
            And(Eq(make_pre_bound_exp fvs1 id1 "l" fun_num branch_trace depth,
              make_pre_bound_exp fvs2 id2 "l" fun_num branch_trace depth),
            Eq(make_pre_bound_exp fvs1 id1 "h" fun_num branch_trace depth,
            make_pre_bound_exp fvs2 id2 "h" fun_num branch_trace depth)) in
          let sl2 = same_range_pre fvs1' fvs2' (depth - 1) in
          And(sl1, 
           idx_pre_bound id1 idx1 fvs1 depth
            (idx_pre_bound id2 idx2 fvs2 depth
             (Imply(Eq(Id idx1, Id idx2), sl2)))) in
      let rec same_range_post fvs2 fvs1_pre depth =
        if depth <= 0 then True
        else
          let idx2 = make_idx_id fun_num id2 depth in
          let fvs2' = idx2::fvs2 in
          let idx1_pre = make_idx_id fun_num id1 depth in
          let fvs1'_pre = idx1_pre::fvs1_pre in
          let sl1 =
            (* id2の所有範囲は変化しない *)
          And(Eq(make_pre_bound_exp fvs1_pre id1 "l" fun_num branch_trace depth,
            make_bound_exp fvs2 id2 "l" fun_num branch_trace depth),
          Eq(make_pre_bound_exp fvs1_pre id1 "h" fun_num branch_trace depth,
            make_bound_exp fvs2 id2 "h" fun_num branch_trace depth)) in
          let sl2 = same_range_post fvs2' fvs1'_pre (depth - 1) in
          And(sl1, 
           idx_bound id2 idx2 fvs2 depth
            (idx_pre_bound id1 idx1_pre fvs1_pre depth
              (Imply(Eq(Id idx2, Id idx1_pre), sl2)))) in
    let idx1_pre = make_idx_id fun_num id1 (depth+1) in
    let fvs1_pre = idx1_pre::fvs in
    let idx2 = make_idx_id fun_num id2 (depth+1) in
    let fvs2 = idx2::fvs in
    And(common depth,
      And(idx_pre_bound id1 idx1_pre fvs (depth+1)
      (idx_pre_bound id2 idx2 fvs (depth+1)
        (Imply(Eq(Id idx1_pre, Add(Id idx2, sl)),same_range_pre fvs1_pre fvs2 depth))),
      idx_pre_bound id1 idx1_pre fvs (depth+1)
        (idx_bound id2 idx2 fvs (depth+1)
          (Imply(Eq(Id idx1_pre, Add(Id idx2, sl)), same_range_post fvs2 fvs1_pre depth)))))
    in
          (* id1の外側の所有権が0の場合 *)
    let make_aliasdAddPtr_smtlib_no_change depth =
      let rec common depth =
        if depth <= 0 then True
        else
          (* 所有権は変化せず *)
          let sl1 = 
            And(Eq(make_pre_own_var id2 fun_num branch_trace depth,
                make_own_var id2 fun_num branch_trace depth),
            Eq(make_own_var id1 fun_num branch_trace depth, Id "0")) in
          let sl2 = common (depth-1) in
          And(sl1, sl2) in
      let rec same_range_post fvs2 depth =
        if depth <= 0 then True
        else
          let idx2 = make_idx_id fun_num id2 depth in
          let fvs2' = idx2::fvs2 in
          let sl1 =
            (* id2の所有範囲は変化しない *)
            And(Eq(make_pre_bound_exp fvs2 id2 "l" fun_num branch_trace depth,
              make_bound_exp fvs2 id2 "l" fun_num branch_trace depth),
            Eq(make_pre_bound_exp fvs2 id2 "h" fun_num branch_trace depth,
              make_bound_exp fvs2 id2 "h" fun_num branch_trace depth)) in
          let sl2 = same_range_post fvs2' (depth-1) in
          And(sl1, 
           idx_bound id2 idx2 fvs depth
           (idx_pre_bound id2 idx2 fvs depth
            sl2)) in
        let idx2 = make_idx_id fun_num id2 (depth+1) in
        let fvs2 = idx2::fvs in
        And(common depth, 
         idx_bound id2 idx2 fvs (depth+1)
          (same_range_post fvs2 depth)) in
    if contains_element eq0_list id2 
    then 
      (* id2の内側の要素の所有権が添え字によって変化する場合 *)
      let id2_0 = id2 ^ "_eq0" in
      let rec make_aliasdAddPtr_smtlib_gather depth =
        if depth <= 0 then True
        else
        (* id1とid2の小さい方の所有権を採用 *)
          let sl1 = 
            Or(And(Leq(make_pre_own_var id1 fun_num branch_trace depth,
              make_pre_own_var id2_0 fun_num branch_trace depth),
            Eq(make_pre_own_var id1 fun_num branch_trace depth,
            make_own_var id2 fun_num branch_trace depth)),
            And(Leq(make_pre_own_var id2_0 fun_num branch_trace depth,
              make_pre_own_var id1 fun_num branch_trace depth),
            Eq(make_pre_own_var id2_0 fun_num branch_trace depth,
            make_own_var id2 fun_num branch_trace depth))) in
          (* id2の所有範囲は変化せず，id1の所有権は0になる *)
          let sl2 = 
            And(Eq(make_pre_bound_exp fvs id2_0 "l" fun_num branch_trace depth,
              make_bound_exp fvs id2 "l" fun_num branch_trace depth),
            And(Eq(make_pre_bound_exp fvs id2_0 "h" fun_num branch_trace depth,
              make_bound_exp fvs id2 "h" fun_num branch_trace depth),
            Eq(make_own_var id1 fun_num branch_trace depth, Id "0"))) in
            (* id1とid2の元の所有範囲は等しい *)
        let sl3 = 
          And(Eq(make_pre_bound_exp fvs id1 "l" fun_num branch_trace depth,
            make_pre_bound_exp fvs id2_0 "l" fun_num branch_trace depth),
          Eq(make_pre_bound_exp fvs id1 "h" fun_num branch_trace depth,
          make_pre_bound_exp fvs id2_0 "h" fun_num branch_trace depth)) in
          (* 再帰処理 *)
        let sl4 = make_aliasdAddPtr_smtlib_gather (depth-1) in
      And(sl1, And(sl2, And(sl3, sl4))) in
            (* id1の外側の所有権が0の場合 *)
    (* let rec make_aliasdAddPtr_smtlib_no_change depth =
      if depth <= 0 then True
      else
        (* 所有権は同じ値 *)
        let sl1 = Eq(make_pre_own_var id2_0 fun_num branch_trace depth,
        make_own_var id2 fun_num branch_trace depth) in
        (* 所有範囲は変化しない *)
        let sl2 =
          And(Eq(make_pre_bound_exp fvs id2_0 "l" fun_num branch_trace depth,
              make_bound_exp fvs id2 "l" fun_num branch_trace depth),
          And(Eq(make_pre_bound_exp fvs id2_0 "h" fun_num branch_trace depth,
            make_bound_exp fvs id2 "h" fun_num branch_trace depth),
          Eq(make_own_var id1 fun_num branch_trace depth, Id "0"))) in
          (* 再帰処理 *)
        let sl3 = make_aliasdAddPtr_smtlib_no_change (depth-1) in
        And(sl1, And(sl2, sl3)) in *)
        let sl1 =
        (* id1とid2の元の所有範囲が分割されている場合 *)
        And(Eq(make_pre_bound_exp fvs id2 "l" fun_num branch_trace depth, Id "0"),
        And(Eq(make_pre_bound_exp fvs id2 "h" fun_num branch_trace depth, Sub(sl, Id "1")),
        And(Eq(make_pre_own_var id1 fun_num branch_trace depth, 
          make_pre_own_var id2 fun_num branch_trace depth),
        And(Eq(make_pre_own_var id2 fun_num branch_trace depth, 
          make_own_var id2 fun_num branch_trace depth),
        And(Eq(make_pre_bound_exp fvs id2 "l" fun_num branch_trace depth,
          make_bound_exp fvs id2 "l" fun_num branch_trace depth),
        Eq(Add(make_pre_bound_exp fvs id2 "h" fun_num branch_trace depth, 
          Add(make_pre_bound_exp fvs id1 "h" fun_num branch_trace depth, Id "1")),
          make_bound_exp fvs id2 "h" fun_num branch_trace depth)))))) in
        (* id1の元の所有権が0の場合 *)
      let sl2 = 
        And(Eq(make_pre_own_var id2 fun_num branch_trace depth,
          make_own_var id2 fun_num branch_trace depth),
        And(Eq(make_pre_bound_exp fvs id2 "l" fun_num branch_trace depth,
          make_bound_exp fvs id2 "l" fun_num branch_trace depth),
        Eq(make_pre_bound_exp fvs id2 "h" fun_num branch_trace depth,
        make_bound_exp fvs id2 "h" fun_num branch_trace depth))) in
        [And(Eq(sl,Id "1"), 
          Or(And(Not(Eq(make_pre_own_var id1 fun_num branch_trace depth, Id "0.")),
            And(sl1, make_aliasdAddPtr_smtlib_gather (depth-1))),
          And(Eq(make_pre_own_var id1 fun_num branch_trace depth, Id "0."),
            And(sl2, make_aliasdAddPtr_smtlib_no_change (depth-1)))));
        Eq(make_own_var id1 fun_num branch_trace depth, Id "0.")] 
    else
    (* id2の内側の要素の所有権が添え字によって変化しない場合の制約 *)
    (* id1とid2の元の所有範囲が共有されている場合 *)
      let sl1 = 
        And(Eq(make_pre_bound_exp fvs id2 "l" fun_num branch_trace depth,
          Add(make_pre_bound_exp fvs id1 "l" fun_num branch_trace depth, sl)),
        And(Eq(make_pre_bound_exp fvs id2 "h" fun_num branch_trace depth,
          Add(make_pre_bound_exp fvs id1 "h" fun_num branch_trace depth, sl)),
        And(Eq(make_own_var id2 fun_num branch_trace depth,
          Add(make_pre_own_var id1 fun_num branch_trace depth, make_pre_own_var id2 fun_num branch_trace depth)),
        And(Eq(make_pre_bound_exp fvs id2 "l" fun_num branch_trace depth,
          make_bound_exp fvs id2 "l" fun_num branch_trace depth),
        Eq(make_pre_bound_exp fvs id2 "h" fun_num branch_trace depth,
          make_bound_exp fvs id2 "h" fun_num branch_trace depth))))) in
      (* id1とid2の元の所有範囲が分割されている場合の制約 *)
      let sl2 =
        And(Eq(make_pre_bound_exp fvs id2 "l" fun_num branch_trace depth, Id "0"),
        And(Eq(make_pre_bound_exp fvs id2 "h" fun_num branch_trace depth, Sub(sl, Id "1")),
        (* 一旦id1とid2の元の所有権は同じと仮定 *)
        And(Eq(make_pre_own_var id1 fun_num branch_trace depth, 
          make_pre_own_var id2 fun_num branch_trace depth),
        And(Eq(make_pre_own_var id2 fun_num branch_trace depth, 
          make_own_var id2 fun_num branch_trace depth),
        And(Eq(make_pre_bound_exp fvs id2 "l" fun_num branch_trace depth,
          make_bound_exp fvs id2 "l" fun_num branch_trace depth),
        Eq(Add(make_pre_bound_exp fvs id2 "h" fun_num branch_trace depth, 
          Add(make_pre_bound_exp fvs id1 "h" fun_num branch_trace depth, Id "1")),
          make_bound_exp fvs id2 "h" fun_num branch_trace depth)))))) in
      (* id1の元の所有権が0の場合の制約 *)
      let sl3 = 
        And(Eq(make_pre_own_var id2 fun_num branch_trace depth,
          make_own_var id2 fun_num branch_trace depth),
        And(Eq(make_pre_bound_exp fvs id2 "l" fun_num branch_trace depth,
          make_bound_exp fvs id2 "l" fun_num branch_trace depth),
        Eq(make_pre_bound_exp fvs id2 "h" fun_num branch_trace depth,
        make_bound_exp fvs id2 "h" fun_num branch_trace depth))) in
      [
      (* Imply(Gt(make_pre_own_var id1 fun_num branch_trace depth, Id "0."), *)
        (* And(Or(sl1, sl2), *)
        And(sl2,
          make_aliasdAddPtr_smtlib_gather (depth-1));
      (* Imply(Eq(make_pre_own_var id1 fun_num branch_trace depth, Id "0."),
        And(sl3, make_aliasdAddPtr_smtlib_no_change (depth-1))); *)
      Eq(make_own_var id1 fun_num branch_trace depth, Id "0")]
  | CDeref (id,_) -> 
    (* let num = *y in ...
    評価後のyの所有権は0より大きい
    評価後のyの所有範囲の下限は0以下
    評価後のyの所有範囲の上限は0以上 *)
    [Gt(make_own_var id fun_num branch_trace 1, Id "0.");
     Leq(make_bound_exp fvs id "l" fun_num branch_trace 1, Id "0"); 
     Geq(make_bound_exp fvs id "h" fun_num branch_trace 1, Id "0")]
  | CLetDeref (id1, id2, pos) -> 
    (* let id1 = *id2 in ... *)
    eq0_list := id2 :: !eq0_list;
    let id1_simpleTy = lookup id1 ty_env in
    let id2_simpleTy = lookup id2 ty_env in
    let id2_0 = id2 ^ "_eq0" in
    let id2_non0 = id2 ^ "_non0" in
    new_id id1 pos branch_trace id1_simpleTy; 
    new_id id2 pos branch_trace id2_simpleTy;
    new_id id2_0 pos branch_trace id2_simpleTy;
    new_id id2_non0 pos branch_trace id2_simpleTy;
    let id1_depth = ref_depth id1_simpleTy in
    let id2_depth = ref_depth id2_simpleTy in
    assert(id2_depth == id1_depth + 1);
    (* id2の所有範囲の下限は0と仮定 *)
    let sl1 = [Gt(make_pre_own_var id2 fun_num branch_trace id2_depth, Id "0");
     Eq(make_pre_bound_exp fvs id2 "l" fun_num branch_trace id2_depth, Id "0"); 
     Geq(make_pre_bound_exp fvs id2 "h" fun_num branch_trace id2_depth, Id "0");
     Eq(make_pre_own_var id2 fun_num branch_trace id2_depth, 
        make_own_var id2_0 fun_num branch_trace id2_depth);
     Eq(make_pre_own_var id2 fun_num branch_trace id2_depth, 
        make_own_var id2_non0 fun_num branch_trace id2_depth);
     Eq(make_bound_exp fvs id2_0 "l" fun_num branch_trace id2_depth, Id "0");
     Eq(make_bound_exp fvs id2_0 "h" fun_num branch_trace id2_depth, Id "0");
     Eq(make_bound_exp fvs id2_non0 "l" fun_num branch_trace id2_depth, Id "0");
     Eq(make_bound_exp fvs id2_non0 "h" fun_num branch_trace id2_depth,
        Sub(make_pre_bound_exp fvs id2 "h" fun_num branch_trace id2_depth, Id "1"))] in
     let sl2 = make_letDeref_smtlib fvs fun_num branch_trace id1 id2 id1_depth in
    sl1 @ sl2
    (* [] *)
  | CApp (fun_name,args,pos) -> 
    (* f(y1, y2, ..., yn) *)
    (* 整数引数の変数名と実引数の式の組を返す
    それ以外は何も返さない *)
    let find_subst param arg = 
      match param, arg with
      | (RawId _ , FTInt _), AExp _ -> []
      | (HashId id, FTInt _), AExp e -> [(id, e)]
      | (_, FTRef _), AId _ -> []
      | _ -> raise ConstrError
    in
    (* 評価前の引数の型と評価後の引数の型を抽出 *)
    let (params_before_eval, params_after_eval) = lookup fun_name !fn_env in
    (* 仮引数名と整数引数に渡された式のリスト *)
    let subst = List.concat (List.map2 find_subst params_before_eval args) in
    (* 関数定義の制約生成 *)
    let rec subst_param_before_eval param arg depth = 
     if depth <= 0 then []
     else
      match param, arg with
      (* x | () ref *)
      | (RawId id_param, FTRef (_,ENull,ENull,_)), AId id ->
        (* 呼び出された関数の通し番号 *)
        let num = lookup fun_name funnames_numberings in
        (* 整数型の仮引数名リスト *)
        let fvs' = List.map fst subst in
        (* 引数xの関数開始時の所有権は0　または
        　　　　(実引数の所有権が関数開始時に必要な所有権以上　かつ
        　　　　実引数の所有範囲の下限が関数開始時に必要な所有範囲の下限以下　かつ
        　　　　実引数の所有範囲の上限が関数開始時に必要な所有範囲の上限以上)　 *)
        let rec common depth =
          if depth <= 0 then True
          else
            let sl1 = Or(Eq(Id "0.", make_own_var_be id_param num "b" depth), 
              Geq(make_own_var id fun_num branch_trace depth, make_own_var_be id_param num "b" depth)) in
            let sl2 = common (depth - 1) in
            And(sl1, sl2) in
        let rec same_range fvs fvs_param depth =
          (* 所有範囲の下限を表すデータ構造 *)
          let sll = make_bound_exp_be fvs' id_param "l"  num "b" depth in
          (* 所有範囲の上限を表すデータ構造 *)
          let slh = make_bound_exp_be fvs' id_param "h"  num "b" depth in
          let idx = make_idx_id fun_num id depth in
          let fvs' = idx::fvs in
          let idx_param = make_idx_id num id_param depth in
          let fvs_param' = idx_param::fvs_param in
          if depth <= 0 then True
          else
            let sl1 = And(Leq(make_bound_exp fvs id "l" fun_num branch_trace depth, smtlib_subst subst sll),
             Geq(make_bound_exp fvs id "h" fun_num branch_trace depth, smtlib_subst subst slh)) in
            let sl2 = same_range fvs' fvs_param' (depth-1) in
            And(sl1, 
              Imply(And(Eq(Id idx, Id idx_param),
                And(make_idx_bound_smtlib id idx fvs fun_num branch_trace depth, 
                smtlib_subst subst (make_idx_bound_smtlib_be id_param idx_param fvs_param num "b" depth))), sl2)) in
        [common depth;
        same_range fvs fvs' depth]
      (* x | () ref (left, right, ownership)の形式の場合 *)
      | (RawId id_param, FTRef (ftype,el,eh,f)), AId id -> 
        (* 引数の篩型中の整数変数を別の式で置き換え *)
        let template = asprintf "i_%d_%s_%s_%dth" fun_num id_param "b" in
        let el = subst_idx_name el template in
        let eh = subst_idx_name eh template in
        let scope_low = exp_to_smtlib (exp_subst subst el) in
        let scope_high = exp_to_smtlib (exp_subst subst eh) in
        (* プログラマ指定の所有権が0　または
        　　　　(実引数の所有権がプログラマ指定の所有権以上　かつ
        　　　　実引数の所有範囲の下限がプログラマ指定の所有範囲の下限以下　かつ
        　　　　実引数の所有範囲の上限がプログラマ指定の所有範囲の上限以上) *)
        if depth <= 1 then
          [Or(Eq(Id "0.", Id (string_of_float f)),
          And(Geq(make_own_var id fun_num branch_trace depth, Id (string_of_float f)),
          And(Leq(make_bound_exp fvs id "l" fun_num branch_trace depth, scope_low),
              Geq(make_bound_exp fvs id "h" fun_num branch_trace depth, scope_high))))]
        else
          [Or(Eq(Id "0.", Id (string_of_float f)),
          And(Geq(make_own_var id fun_num branch_trace depth, Id (string_of_float f)),
          And(Leq(make_bound_exp fvs id "l" fun_num branch_trace depth, scope_low),
          And(Geq(make_bound_exp fvs id "h" fun_num branch_trace depth, scope_high),
          List.hd (subst_param_before_eval (RawId id_param, ftype) arg (depth-1))))))]
        (* 整数の時は何もしない *)
      | _, AExp _ -> []
      | _ -> raise ConstrError
    in
    (* 関数引数に対するsmtlibの条件式の生成 *)
    let find_depth ftype =
      let rec iterative_find_depth ftype depth =
        match ftype with
        | FTRef (ftype',_ ,_ ,_) -> iterative_find_depth ftype' (depth+1)
        | FTInt _ -> depth
      in iterative_find_depth ftype 0
    in 
    let before_depths = List.map (fun x -> find_depth (snd x)) params_before_eval in
    let rec my_map func a_list b_list c_list =
      match a_list, b_list, c_list with
      | [], [], [] -> []
      | a_hd::a_tl, b_hd::b_tl, c_hd::c_tl -> (func a_hd b_hd c_hd) :: (my_map func a_tl b_tl c_tl)
      | _ -> raise (Error "my_map error")
    in
    let constraints1 = List.concat (my_map subst_param_before_eval params_before_eval args before_depths) in
    (* subst_param_before_evalとほぼ同様,呼び出した関数の評価終了時の制約 *)
    let rec subst_param_after_eval ftid_ft arg depth = 
      if depth <= 0 then []
      else
        match ftid_ft, arg with
        | (RawId id_param, FTRef (_,ENull,ENull,_)), AId id ->
          remove_element eq0_list id;
          let num = lookup fun_name funnames_numberings in
          (* let fvs' = union_list (List.map fst subst) fvs in *)
          let fvs' = List.map fst subst in
          (* 関数評価後は関数の返り値の型の所有表現と等しい *)
          let rec common depth =
            if depth <= 0 then True
            else
              let sl1 = Eq(make_own_var id fun_num branch_trace depth, make_own_var_be id_param num "e" depth) in
              let sl2 = common (depth - 1) in
              And(sl1, sl2) in
          let rec same_range fvs fvs_param depth =
            (* 所有範囲の下限を表すデータ構造 *)
            let sll = make_bound_exp_be fvs' id_param "l"  num "e" depth in
            (* 所有範囲の上限を表すデータ構造 *)
            let slh = make_bound_exp_be fvs' id_param "h"  num "e" depth in
            let idx = make_idx_id fun_num id depth in
            let fvs' = idx::fvs in
            let idx_param = make_idx_id num id_param depth in
            let fvs_param' = idx_param::fvs_param in
            if depth <= 0 then True
            else
              let sl1 = And(Eq(make_bound_exp fvs id "l" fun_num branch_trace depth, smtlib_subst subst sll),
                Eq(make_bound_exp fvs id "h" fun_num branch_trace depth, smtlib_subst subst slh)) in
              let sl2 = same_range fvs' fvs_param' (depth-1) in
              And(sl1, 
                Imply(And(Eq(Id idx, Id idx_param),
                  And(make_idx_bound_smtlib id idx fvs fun_num branch_trace depth,
                  smtlib_subst subst (make_idx_bound_smtlib_be id_param idx_param fvs_param num "e" depth))), sl2)) in
          (* 引数のvar_locationsを生成 *)
          let simpleTy = depth_to_simpleTy depth in
          new_id id pos branch_trace simpleTy;
          [common depth;
          same_range fvs fvs' depth]
        | (RawId id_param, FTRef (ftype,el,eh,f)), AId id -> 
          remove_element eq0_list id;
          let template = asprintf "i_%d_%s_%s_%dth" fun_num id_param "e" in
          let el = subst_idx_name el template in
          let eh = subst_idx_name eh template in
          let l_arg_exp = exp_subst subst el in
          let h_arg_exp = exp_subst subst eh in
          (* print_exp h_arg_exp; *)
          (* 引数のvar_locationsを生成 *)
          let simpleTy = depth_to_simpleTy depth in
          new_id id pos branch_trace simpleTy;
          let coeff_map_h = coeffs h_arg_exp in
          let coeff_map_l = coeffs l_arg_exp in
          let find_coeff h_or_l coeff_map = 
            let look_up' var_name fv =
              try
                let coeff = lookup fv coeff_map in
                Eq(Id var_name, coeff)
              with
              | _ -> Eq(Id var_name, Id "0")  in
            let id_pos = lookup_pos id branch_trace !var_locations in
            let var_name_d = asprintf "d_%d_%s_%s_%d%a_%d" fun_num h_or_l id id_pos pp_branch_trace branch_trace depth in
            (look_up' var_name_d "") ::
            List.map
            (fun fv ->
              let var_name = asprintf "c_%d_%s_%s_%s_%d%a_%d" fun_num h_or_l fv id id_pos pp_branch_trace branch_trace depth in
              look_up' var_name fv) fvs in
          let sl1 = [Eq(make_own_var id fun_num branch_trace depth, Id (string_of_float f));]
          @ find_coeff "h" coeff_map_h 
          @ find_coeff "l" coeff_map_l in
          let sl2 = subst_param_after_eval (RawId id_param, ftype) arg (depth-1) in
          sl1 @ sl2
        | _, AExp _ -> []
        | _ -> raise (Error (sprintf "subst_param_after_eval error"))
    in
    (* 関数評価後に対するsmtlibの条件式の生成 *)
    let after_depths = List.map (fun x -> find_depth (snd x)) params_after_eval in
    let constraints2 = List.concat (my_map subst_param_after_eval params_after_eval args after_depths) in
    (* 関数評価前と評価後の条件式を結合 *)
    constraints1 @ constraints2
    (* alias(id1 = *id2); ... *)
  | CAliasDeref (id1, id2, pos) -> 
      remove_element eq0_list id2;
      let id1_simpleTy = lookup id1 ty_env in
      let id2_simpleTy = lookup id2 ty_env in
      let id2_0 = id2 ^ "_eq0" in
      let id2_non0 = id2 ^ "_non0" in
      new_id id1 pos branch_trace id1_simpleTy; 
      new_id id2 pos branch_trace id2_simpleTy;
      let id1_depth = ref_depth id1_simpleTy in
      let id2_depth = ref_depth id2_simpleTy in
      assert(id1_depth + 1 == id2_depth);
      (* 一番外側の所有範囲の制約 
      id2の先頭は読み出し可能
      外側の所有権は変化しない　*)
      let sl1 = 
        [
        Gt(make_own_var id2_0 fun_num branch_trace id2_depth, Id "0.");
        Eq(make_own_var id2_0 fun_num branch_trace id2_depth, 
          make_own_var id2 fun_num branch_trace id2_depth);
        Eq(make_own_var id2_0 fun_num branch_trace id2_depth,
          make_own_var id2_non0 fun_num branch_trace id2_depth);
        Eq(make_bound_exp fvs id2 "l" fun_num branch_trace id2_depth, Id "0");
        Eq(make_bound_exp fvs id2 "h" fun_num branch_trace id2_depth, 
          Add(make_bound_exp fvs id2_non0 "h" fun_num branch_trace id2_depth, Id "1"));
        ] in
      let sl2 = make_aliasDeref_smtlib fvs fun_num branch_trace id1 id2 id2_depth in
      sl1 @ sl2
  | _ -> raise ConstrError
  
(* 関数仮引数のうち#付き整数引数名を返す *)
let find_intv param = 
  match param with
  | (HashId id, FTInt _) -> [id]
  | _ -> []

(* 関数仮引数のうち参照型である場合はその引数名を返す *)
let find_ref_id param = 
  match param with
  | (RawId id, FTRef _) -> [id]
  | _ -> [] 

(* 関数仮引数のうち参照型である引数の所有権の加減，添え字の上限，所有権の組を返す
こんなに周りくどいやり方する必要ある？？？？ *)
let rec find_own_annotation ref_id params = 
  match params with
  | (RawId id, ftype) :: _ when id = ref_id -> ftype
  | _ :: params' -> find_own_annotation ref_id params'
  | [] -> raise Not_found

(* ある関数全体の制約をsmtlib形式に直す
funid_constr:関数名とその関数内の制約のリストの組み
fun_num:関数の通し番号
funnames_numberings:関数名とその関数の通し番号の組のリスト *)
let fun_constrs_to_smtlib funname_constrs fun_num funnames_numberings =
  (* 関数名と制約を抽出 *)
  let (fun_name, constrs) = funname_constrs in
  (* 評価前と評価後の関数の引数の型を抽出 *)
  let (params_before_eval, params_after_eval) = lookup fun_name !fn_env in
  (* 関数の単純型環境 *)
  let ty_env = lookup fun_name !all_tyenv in
  (* 関数引数のうち整数変数名を抽出 *)
  let fvs = List.concat (List.map find_intv params_before_eval) in
  (* let _ = List.map (fun x -> print_string (x ^ " ")) fvs in *)
  (* print_string "\n";flush stdout; *)
  (* 関数仮引数のうち参照型である引数名を抽出 *)
  let ref_ids = List.concat (List.map find_ref_id params_before_eval) in
  (* 関数引数名をvar_locations(変数id, (変数の位置, branch_trace, 単純型))のリストに代入 *)
  var_locations := 
  List.map (fun id -> 
    let simple_ty = lookup id ty_env in
    (id, (0, [], simple_ty))) ref_ids;
  (* smtlibで関数宣言するために必要な情報を初期化 *)
  varown_count := [];
  (* 関数開始時の制約を生成する関数，id:変数名 *)
  let ref_id_before_eval_to_smtlibs id = 
    let ftype = find_own_annotation id params_before_eval in 
    (* smtlibに渡すためvarown_countを更新 *)
    let depth = ftref_depth ftype in
    varown_count := (id, "b", fun_num, depth) :: !varown_count;
    let rec ref_id_before_eval_to_smtlibs_sub ftype fvs =
    match ftype with
    | FTInt _ -> [], []
    | FTRef (ftype', exp_low, exp_high, own) ->
      let depth = ftref_depth (FTRef (ftype', exp_low, exp_high, own)) in
      let idx = make_idx_id fun_num id depth in
      let fvs' = idx::fvs in
      match exp_low with
      (* 所有権指定がない場合 *)
      | ENull ->
        (* 関数引数の最初の所有権と当初決まっている所有権は等しい
        関数引数の最初の所有範囲の下限と当初決まっている所有範囲の下限は等しい
        関数引数の最初の所有範囲の上限と当初決まっている所有範囲の上限は等しい *)
        let sl1, sl2 = 
        [Eq(make_own_var id fun_num [] depth, make_own_var_be id fun_num "b" depth);
        Geq(make_own_var_be id fun_num "b" depth, Id "0.");
        Leq(make_own_var_be id fun_num "b" depth, Id "1.")],
        [Eq(make_bound_exp fvs id "l" fun_num [] depth, make_bound_exp_be fvs id "l" fun_num "b" depth);
        Eq(make_bound_exp fvs id "h" fun_num [] depth, make_bound_exp_be fvs id "h" fun_num "b" depth);
        ] in
        let sl3, sl4 = (ref_id_before_eval_to_smtlibs_sub ftype' fvs') in
        sl1 @ sl3, 
        sl2 @
        (List.map 
          (fun x -> Imply(
            And(make_idx_bound_smtlib id idx fvs fun_num [] depth,
              make_idx_bound_smtlib_be id idx fvs fun_num "b" depth), x)) sl4)
        (* 所有権指定がある場合 *)
      | _ ->
        let template = asprintf "i_%d_%s_%s_%dth" fun_num id "b" in
        let exp_low = subst_idx_name exp_low template in
        let exp_high = subst_idx_name exp_high template in
        let coeff_map_h = coeffs exp_high in
        let coeff_map_l = coeffs exp_low in
        let find_coeff h_or_l coeff_map =
          let look_up' var_name fv =
            try
              let coeff = lookup fv coeff_map in
              Eq(Id var_name, coeff)
            with
            | _ -> Eq(Id var_name, Id "0")  in
          let var_name_d = asprintf "d_%d_%s_%s_%s_%d" fun_num h_or_l id "b" depth in
          (look_up' var_name_d "") ::
          (List.map
            (fun fv -> 
              let var_name = asprintf "c_%d_%s_%s_%s_%s_%d" fun_num h_or_l fv id "b" depth in
              look_up' var_name fv) fvs) in
        (* 関数引数の最初の所有権と所有権の指定は等しい
        関数引数の最初の所有範囲の下限と所有範囲の下限の指定は等しい
        関数引数の最初の所有範囲の下限と所有範囲の下限の指定は等しい *)
        let sl1, sl2 = 
        [Eq(make_own_var id fun_num [] depth, Id (string_of_float own));
        Eq(make_own_var_be id fun_num "b" depth, Id (string_of_float own));],
        [
        Eq(make_bound_exp fvs id "l" fun_num [] depth, exp_to_smtlib exp_low);
        Eq(make_bound_exp fvs id "h" fun_num [] depth, exp_to_smtlib exp_high);
        ] 
        @ find_coeff "h" coeff_map_h 
        @ find_coeff "l" coeff_map_l in
        let sl3, sl4 = (ref_id_before_eval_to_smtlibs_sub ftype' fvs') in
        sl1 @ sl3, 
        sl2 @
        (List.map 
          (fun x -> Imply(
            And(make_idx_bound_smtlib id idx fvs fun_num [] depth,
              And(Geq(Id idx, exp_to_smtlib exp_low), Leq(Id idx, exp_to_smtlib exp_high))), x)) sl4)
  in 
  let sl, sl2 = ref_id_before_eval_to_smtlibs_sub ftype fvs in
  sl @ sl2 in
  (* 関数仮引数のうち#がついていない　かつ　参照型である引数集合の制約を生成 *)
  let smtlibs_before_eval = List.concat (List.map ref_id_before_eval_to_smtlibs ref_ids) in
  (* 関数内部の制約をsmtlibの形式に変換 *)
  let constr_to_smtlib_with_error_output c = 
    try constr_to_smtlib fvs fun_num funnames_numberings [] ty_env c with
    | Error s -> 
      p c;
    raise (Error ("generateSmtlibConstraint miss: "^s))  in
  let smtlibs_among_eval = List.concat (List.map (constr_to_smtlib_with_error_output) constrs) in
  (* let smtlibs_among_eval = List.concat (List.map (constr_to_smtlib fvs fun_num funnames_numberings [] ty_env) constrs) in *)
  (* 関数終了時の制約を生成する関数，id:変数名 *)
  (* print_string "3\n"; flush stdout; *)
  let ref_id_after_eval_to_smtlibs id = 
    let ftype = find_own_annotation id params_after_eval in 
    (* smtlibに渡すためvarown_countを更新 *)
    let depth = ftref_depth ftype in
    varown_count := (id, "e", fun_num,depth) :: !varown_count;
    let rec ref_id_after_eval_to_smtlibs_sub ftype fvs =
    match ftype with
    | FTInt _ -> [], []
    | FTRef (ftype', el2, eh2, f2) ->
    let depth = ftref_depth (FTRef (ftype', el2, eh2, f2)) in
    let idx = make_idx_id fun_num id depth in
    let fvs' = idx::fvs in
    match el2 with
    | ENull ->
      (* 評価終了時の引数の所有権は0　または
      　　　　(評価終了時の引数の所有権がその時の所有権以下　かつ
      　　　　評価終了時の引数の所有範囲の下限がその時の所有範囲の下限以上　かつ
      　　　　評価終了時の引数の所有範囲の上限がその時の所有範囲の上限以下) *)
      let sl1, sl2 = 
      [Geq(make_own_var_be id fun_num "e" depth, Id "0.");
      Leq(make_own_var_be id fun_num "e" depth, Id "1.")],
      [Or(Eq(make_own_var_be id fun_num "e" depth, Id "0."),
       And(Leq(make_own_var_be id fun_num "e" depth, make_own_var id fun_num [] depth),
       And(Geq(make_bound_exp_be fvs id "l" fun_num "e" depth, make_bound_exp fvs id "l" fun_num [] depth),
           Leq(make_bound_exp_be fvs id "h" fun_num "e" depth, make_bound_exp fvs id "h" fun_num [] depth))))] 
      in
      let sl3, sl4 = ref_id_after_eval_to_smtlibs_sub ftype' fvs' in
      sl1 @ sl2,
      sl3 @
      List.map 
      (fun x -> 
        Imply(
          And(make_idx_bound_smtlib id idx fvs fun_num [] depth,
            make_idx_bound_smtlib_be id idx fvs fun_num "e" depth), x)) sl4
    | _ ->
      let template = asprintf "i_%d_%s_%s_%dth" fun_num id "e" in
      let el2 = subst_idx_name el2 template in
      let eh2 = subst_idx_name eh2 template in
      let coeff_map_h = coeffs eh2 in
      let coeff_map_l = coeffs el2 in
      let find_coeff h_or_l coeff_map =
        let look_up' var_name fv =
          try
            let coeff = lookup fv coeff_map in
            Eq(Id var_name, coeff)
          with
          | _ -> Eq(Id var_name, Id "0")  in
        let var_name_d = asprintf "d_%d_%s_%s_%s_%d" fun_num h_or_l id "e" depth in
        (look_up' var_name_d "") ::
        (List.map
          (fun fv -> 
            let var_name = asprintf "c_%d_%s_%s_%s_%s_%d" fun_num h_or_l fv id "e" depth in
            look_up' var_name fv) fvs) in
      (* 評価終了時の引数のプログラマ指定の所有権は0　または
      　　　　(評価終了時の引数のプログラマ指定の所有権がその時の所有権以下　かつ
      　　　　評価終了時の引数のプログラマ指定の所有範囲の下限がその時の所有範囲の下限以上　かつい
      　　　　評価終了時の引数のプログラマ指定の所有範囲の上限がその時の所有範囲の上限以下) *)
      let sl1, sl2 = 
      [Or(Eq(Id (string_of_float f2), Id "0."),
       Leq(Id (string_of_float f2), make_own_var id fun_num [] depth));
       Eq(make_own_var_be id fun_num "e" depth, Id (string_of_float f2));],
      [Geq(exp_to_smtlib el2, make_bound_exp fvs id "l" fun_num [] depth);
      (* Eq(exp_to_smtlib el2, make_bound_exp_be fvs_e id "l" fun_num "e" depth); *)
      Leq(exp_to_smtlib eh2, make_bound_exp fvs id "h" fun_num [] depth);
      (* Eq(exp_to_smtlib eh2, make_bound_exp_be fvs_e id "h" fun_num "e" depth) *)
      ]
      @ find_coeff "h" coeff_map_h 
      @ find_coeff "l" coeff_map_l in
      let sl3, sl4 = ref_id_after_eval_to_smtlibs_sub ftype' fvs' in
      sl1 @ sl2,
      sl3 @
      List.map 
      (fun x -> 
        Imply(
          And(make_idx_bound_smtlib id idx fvs fun_num [] depth,
            And(Geq(Id idx, exp_to_smtlib el2), Leq(Id idx, exp_to_smtlib eh2))), x)) sl4
  in 
  let sl1, sl2 = ref_id_after_eval_to_smtlibs_sub ftype fvs in
  sl1 @ sl2 in
  (* 評価後の関数仮引数のうち参照型である引数集合の制約を生成 *)
  let smtlibs_after_eval = List.concat (List.map ref_id_after_eval_to_smtlibs ref_ids) in
  (* 任意の変数の任意の位置における所有権が0以上1以下である制約を付加する関数 *)
  let make_scope_limit_smtlib (id, (pos,branch_trace, simpleTy)) =
    let depth = ref_depth simpleTy in
    let rec make_scope_limit_smtlib_sub depth =
      if depth <= 0 then []
      else
        let var_name = asprintf "o_%d_%s_%d%a_%d" fun_num id pos pp_branch_trace branch_trace depth in
        let o_id = Id(var_name) in
        [Geq(o_id, Id "0.");
        Leq(o_id, Id "1.")]
        @ make_scope_limit_smtlib_sub (depth-1)
    in make_scope_limit_smtlib_sub depth 
  in
  (* 所有権の値の範囲の制約生成 *)
  let scope_limit_smtlib = List.concat (List.map make_scope_limit_smtlib !var_locations) in 
  (scope_limit_smtlib @ smtlibs_before_eval @ smtlibs_among_eval @ smtlibs_after_eval, !var_locations, !varown_count, fvs)

(* プログラム全体の制約から関数名とその関数の順番の組のリストを返す関数 *)
let numbering_function all_cs = 
  let rec iterative_numbering_function all_cs cnt res =
    match all_cs with
    | [] -> res
    | ics :: all_cs' -> 
      let (id,_) = ics in
      iterative_numbering_function all_cs' (cnt+1) ((id, cnt) :: res)
  in iterative_numbering_function all_cs 0 []

(* 関数の制約生成及び，制約の書き出しに必要な情報を返す関数 *)
let all_cs_to_smtlib all_cs flag fun_num =
  (* 関数名とその順番の組のリストを作成 *)
  let funnames_numberings = numbering_function all_cs in
  (* fun_num番目の関数の制約，var_locations, varown_count, 自由変数の集合 *)
  let (smtlibs, var_locations, varown_count, fvs) = fun_constrs_to_smtlib (List.nth all_cs fun_num) fun_num funnames_numberings in
  let ss' = 
    (* flag次第でおそらく一つも成り立たない？？？？？ *)
    if flag then 
      [Not(Ands smtlibs)]
    else
      smtlibs
  in
  (* let _ = List.map (fun (id1, (pos, _, _)) -> Format.printf "%s %d | " id1 pos) var_locations in
  print_string "\n"; *)
  (var_locations, varown_count, fvs, ss')
    