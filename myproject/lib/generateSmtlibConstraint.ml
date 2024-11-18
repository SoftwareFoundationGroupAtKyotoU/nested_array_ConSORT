(* Module for ownership constraint generation(smtlib acceptable form) *)

open OwnConstraintSyntax
open SmtlibSyntax
open Util
open Format
open CollectOwnConstraint
open Syntax

exception Unbound
exception ConstrError

(* 変数idとその変数の存在する場所 
(変数id, (変数の場所pos, branch_trace))のリスト*)
(* branch_traceはif文の分岐情報を表す *)
let var_locations = ref []

(* 環境からidとbranch_traceに対応する関数内の位置posを返す *)
(* branch_traceはif文の分岐情報を表す *)
let rec lookup_pos id branch_trace env =
  match env with
  | [] -> raise Unbound
  | (x, (position, branch_trace')) :: left_env -> 
    if id = x && branch_trace = branch_trace' then position else lookup_pos id branch_trace left_env

(* 環境からidとbranch_traceに対応する関数内の直前の位置posを返す *)
let rec lookup_pre_pos id branch_trace env = 
  match env with
  | [] -> raise Unbound
  | (x, _) :: left_env -> if id = x then lookup_pos id branch_trace left_env else lookup_pre_pos id branch_trace left_env

(* var_locationsに新しい変数idを追加または既存のidの情報を更新 *)
let new_id id position branch_trace =
  try
    let pos' = lookup_pos id branch_trace !var_locations in
    if pos' = position then ()
    else
      var_locations := (id, (position, branch_trace)) :: !var_locations
  with 
    Unbound -> var_locations := (id, (position, branch_trace)) :: !var_locations
    

(* resは返り値用のリスト，末尾再帰のため？
var_locations((変数id, (関数内での場所, branch_trace))のリスト)から
branch_traceの分岐が同じ変数idを重複なしでリストにして全て返す関数 *)
let rec collect_same_trace_vars branch_trace var_locations = 
  let rec iterative_collect_same_trace_vars branch_trace var_locations res =
    (match var_locations with
    | [] -> res
    | (x, (_,lst)) :: var_locations' -> 
      if List.mem x res || branch_trace <> lst then 
        iterative_collect_same_trace_vars branch_trace var_locations' res 
      else 
        iterative_collect_same_trace_vars branch_trace var_locations' (x :: res))
  in iterative_collect_same_trace_vars branch_trace var_locations []

(* id fun_num branch_traceを元にsmtlibに渡す所有権の変数を生成 *)
let make_own_var id fun_num branch_trace = 
  let var_pos = lookup_pos id branch_trace !var_locations in
  let var_name = asprintf "o_%d_%s_%d_%a" fun_num id var_pos pp_branch_trace branch_trace in 
  Id(var_name)
  (* Id("o_" ^ (string_of_int fun_num) ^ "_" ^ id ^ "_" ^ (string_of_int (lookup_pos id branch_trace !var_locations)) ^ (branch_trace_to_str branch_trace)) *)

(* id, fun_num branch_traceを元にsmtlibに渡す所有権の変数を生成 
      引数が表す場所での変数の直前の所有権の変数を表している*)
let make_pre_own_var id fun_num branch_trace = 
  let var_pre_pos = lookup_pre_pos id branch_trace !var_locations in
  let var_name = asprintf "o_%d_%s_%d_%a" fun_num id var_pre_pos pp_branch_trace branch_trace in 
  Id(var_name)

(* 関数評価の最初と最後の状態での所有権を表す
b_or_eはbまたはeでbeginとendの意 *)
let make_own_var_be id fun_num b_or_e = 
  let var_name = asprintf "o_%d_%s_%s" fun_num id b_or_e in
  Id(var_name)

(* 所有範囲の下限を定める (cかd)_(fun_num)_l_(fv)_(id)_(pos)_(branch_trace)
  nって何，idはどの変数の所有権を計算しているか？
  fvs=["a", "b"], id="x", fun_num=1, branch_trace=[then]の場合
    Add(Mul(Id( "c_1_l_a_x_pos?_then"), FV(a) ), Add(Mul(Id("c_1_l_b_x_?_then"), FV(b)), "d_1_l_x_pos?_then") )
    つまり "c_1_l_a_x_pos?_then" * FV(a) + "c_1_l_b_x_pos?_then" * FV(b) + "d_1_l_x_pos?_then"
  *)
let rec make_low_bound_exp fvs id fun_num branch_trace = 
  let id_pos = lookup_pos id branch_trace !var_locations in
  match fvs with
  | [] -> 
    let var_name = asprintf "d_%d_l_%s_%d_%a" fun_num id id_pos pp_branch_trace branch_trace in
    Id(var_name)
  | fv :: fvs' ->
    let var_name = asprintf "c_%d_l_%s_%s_%d_%a" fun_num fv id id_pos pp_branch_trace branch_trace in
    Add( Mul(Id(var_name), FV(fv)), make_low_bound_exp fvs' id fun_num branch_trace)

(* 所有範囲の上限を定める (cかd)_(fun_num)_h_(fv)_(id)_(pos)_(branch_trace)
  fvs=["a", "b"], id="x", fun_num=1, branch_trace=Thenの場合
    Add(Mul(Id( "c_1_h_a_x_pos?_then"), FV(a) ), Add(Mul(Id("c_1_h_b_x_pos?_then"), FV(b)), "d_1_h_x_pos?_then") )
    つまり "c_1_h_a_x_pos?_then" * FV(a) + "c_1_h_b_x_pos?_then" * FV(b) + "d_1_h_x_pos?_then"*)
let rec make_high_bound_exp fvs id fun_num branch_trace = 
  let id_pos = lookup_pos id branch_trace !var_locations in
  match fvs with
  | [] -> 
    let var_name = asprintf "d_%d_h_%s_%d_%a" fun_num id id_pos pp_branch_trace branch_trace in
    Id(var_name)
  | fv :: fvs' ->
    let var_name = asprintf "c_%d_h_%s_%s_%d_%a" fun_num fv id id_pos pp_branch_trace branch_trace in
    Add( Mul(Id(var_name), FV(fv)), make_high_bound_exp fvs' id fun_num branch_trace)

let rec make_bound_exp fvs id h_or_l fun_num branch_trace = 
  let id_pos = lookup_pos id branch_trace !var_locations in
  match fvs with
  | [] -> 
    let var_name = asprintf "d_%d_%s_%s_%d_%a" fun_num id h_or_l id_pos pp_branch_trace branch_trace in
    Id(var_name)
  | fv :: fvs' ->
    let var_name = asprintf "c_%d_%s_%s_%s_%d_%a" fun_num fv id h_or_l id_pos pp_branch_trace branch_trace in
    Add( Mul(Id(var_name), FV(fv)), make_bound_exp fvs' id h_or_l fun_num branch_trace)

(*直前の所有範囲の下限,または上限を環境変数の一次式で表す *)
let rec make_pre_bound_exp fvs id h_or_l fun_num branch_trace = 
  let id_pre_pos = lookup_pre_pos id branch_trace !var_locations in
  match fvs with
  | [] -> 
    let var_name = asprintf "d_%d_%s_%s_%d_%a" fun_num id h_or_l id_pre_pos pp_branch_trace branch_trace in
    Id(var_name)
  | fv :: fvs' ->
    let var_name = asprintf "c_%d_%s_%s_%s_%d_%a" fun_num fv id h_or_l id_pre_pos pp_branch_trace branch_trace in
    Add(Mul(Id(var_name), FV(fv)), make_pre_bound_exp fvs' id h_or_l fun_num branch_trace)

(* 関数評価の最初と最後の状態での所有範囲の上限または下限を表す
b_or_eはbまたはeでbeginとendの意 *)
let rec make_bound_exp_be fvs id h_or_l fun_num b_or_e = 
  match fvs with
  | [] -> 
    let var_name = asprintf "d_%d_%s_%s_%s" fun_num h_or_l id b_or_e in
    Id(var_name)
  | fv :: fvs' ->
    let var_name = asprintf "c_%d_%s_%s_%s_%s" fun_num h_or_l fv id b_or_e in
    Add(Mul(Id(var_name), FV(fv)), make_bound_exp_be fvs' id h_or_l fun_num b_or_e)

(* 二つの参照の所有範囲の下限と上限を受け取り
その範囲が等しいという制約を返す関数 *)
let make_same_scope_smtlib id1_low id1_high id2_low id2_high = 
  And(Eq(id1_low, id2_low), Eq(id1_high, id2_high))

(* 二つの参照の所有範囲の上限と下限を受け取り
その範囲が隣接しているという制約を返す関数 *)
let make_adjacent_scope_smtlib id1_high id2_low =
  Eq(Add(id1_high, Id "1"), id2_low)

(* smtlibで変数宣言するために必要そう？
変数のid, b or e, 関数の通し番号の組 *)
let varown_count = ref []

(** Main procedure for generating the ownership constraints 
オーナーシップ制約生成のためのメイン手続き
fvs: 関数引数のうちint型である変数の名前
n: 関数の通し番号
funnames_numberings: 関数名と通し番号の組のリスト
branch_trace: if節のどちらを通ってきたかを表す文字列のリスト
c: 制約のリスト
関数内の所有権は関数の整数引数とインデックスにしか依存できない？
*)
let rec constr_to_smtlib fvs fun_num funnames_numberings branch_trace c =
  match c with
  | CIf (e,cs1,cs2,pos) -> 
    (* if式直前の変数idのリスト *)
    let ids_pre = collect_same_trace_vars branch_trace !var_locations in 
    (* var_locations((変数id, (所有権のレベルl, branch_trace))のリスト)にthenブランチとelseブランチに対応する変数idが追加 *)
    List.iter (fun id -> new_id id pos (Then :: branch_trace); new_id id pos (Else :: branch_trace)) ids_pre;
    let constraints_pre = 
      (* 制約のリスト[Eq(...); Eq(...); ..., Eq(...)]を作る *)
      List.concat (List.map 
        (fun id -> 
          (* if式の制約
           if直前，then節に入った時，else節に入った時の所有権は等しい
           if直前，then節に入った時，else節に入った時の所有範囲の下限は等しい
           if直前，then節に入った時，else節に入った時の所有範囲の上限は等しい*)
          [Eq(make_own_var id fun_num branch_trace, make_own_var id fun_num (Then :: branch_trace));
           Eq(make_own_var id fun_num branch_trace, make_own_var id fun_num (Else :: branch_trace));
           Eq(make_bound_exp fvs id "l" fun_num branch_trace, make_bound_exp fvs id "l" fun_num (Then :: branch_trace));
           Eq(make_bound_exp fvs id "l" fun_num branch_trace, make_bound_exp fvs id "l" fun_num (Else :: branch_trace));
           Eq(make_bound_exp fvs id "h" fun_num branch_trace, make_bound_exp fvs id "h" fun_num (Then :: branch_trace));
           Eq(make_bound_exp fvs id "h" fun_num branch_trace, make_bound_exp fvs id "h" fun_num (Else :: branch_trace))]
        ) ids_pre) in
    (* then節側の制約をsmtlibが読める制約の形に直す *)
    let constraints1 = List.concat (List.map (constr_to_smtlib fvs fun_num funnames_numberings (Then :: branch_trace)) cs1) in
    (* 条件式が成り立つならばthen節の制約が成り立つ，という形に変更 *)
    let constraints1' = List.map (fun s -> Imply(exp_to_smtlib e, s)) constraints1 in
    (* else節側の制約をsmtlibが読める制約の形に直す *)
    let constraints2 = List.concat (List.map (constr_to_smtlib fvs fun_num funnames_numberings (Else :: branch_trace)) cs2) in
    (* 条件式が成り立たないならばelse節の制約が成り立つ，という形に変更 *)
    let constraints2' = List.map (fun s -> Imply(Not(exp_to_smtlib e), s)) constraints2 in
    (* then節評価後の変数のリスト *)
    let ids_post_if = collect_same_trace_vars (Then :: branch_trace) !var_locations in
    (* else節評価後の変数のリスト *)
    let ids_post_el = collect_same_trace_vars (Else :: branch_trace) !var_locations in
    (* then節とelse節評価後の変数のリストを結合 *)
    let ids_post = union_list ids_post_if ids_post_el in
    (* then節とelse節評価後の各変数について，レベルを1増やしてvar_locationsに追加 *)
    List.iter (fun id -> new_id id (pos+1) branch_trace) ids_post; 
    let constraints_post_if = 
      List.map 
        (fun s -> Imply(exp_to_smtlib e, s))
        (List.concat (List.map 
        (* then式評価後の各変数について 
            条件式がなりたつならば
          　　評価前の所有権が0と等しい　または
                (評価前の所有権はthen節評価時の所有権以下　かつ
                評価前の所有範囲の下限はthen節評価時の所有範囲の下限以上　かつ
                評価前の所有範囲の上限はthen節評価時の所有範囲の上限以下) *)
          (fun id -> 
            [Or(Eq(make_own_var id fun_num branch_trace, Id "0."),
             And(Leq(make_own_var id fun_num branch_trace, make_own_var id fun_num (Then :: branch_trace)),
             And(Geq(make_bound_exp fvs id "l" fun_num branch_trace, make_bound_exp fvs id "l" fun_num (Then :: branch_trace)),
                 Leq(make_bound_exp fvs id "h" fun_num branch_trace, make_bound_exp fvs id "h" fun_num (Then :: branch_trace)))))]
          ) ids_post_if)) in
    let constraints_post_el = 
      List.map 
        (fun s -> Imply(Not(exp_to_smtlib e), s))
        (* else式評価後の各変数について 
            条件式がなりたたないならば
          　　評価前の所有権が0と等しい　または
                (評価前の所有権はelse節評価時の所有権以下　かつ
                評価前の所有範囲の下限はelse節評価時の所有範囲の下限以上　かつ
                評価前の所有範囲の上限はelse節評価時の所有範囲の上限以下) *)
        (List.concat (List.map 
          (fun id -> 
            [Or(Eq(make_own_var id fun_num branch_trace, Id "0."),
             And(Leq(make_own_var id fun_num branch_trace, make_own_var id fun_num (Else :: branch_trace)),
             And(Geq(make_bound_exp fvs id "l" fun_num branch_trace, make_bound_exp fvs id "l" fun_num (Else :: branch_trace)),
                 Leq(make_bound_exp fvs id "h" fun_num branch_trace, make_bound_exp fvs id "h" fun_num (Else :: branch_trace)))))]
          ) ids_post_el)) in
    (* 制約をつなげて返す *)
    constraints_pre @ constraints1' @ constraints2' @ constraints_post_if @ constraints_post_el
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
    new_id id1 pos branch_trace; 
    new_id id2 pos branch_trace;
    (* numをsmtlibの読める形に変形 *)
    let sl = exp_to_smtlib e in
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
    [Or(Geq(make_pre_own_var id2 fun_num branch_trace, Add(make_own_var id1 fun_num branch_trace, make_own_var id2 fun_num branch_trace)),
     Or(Gt(Add(make_bound_exp fvs id1 "l" fun_num branch_trace, sl), make_bound_exp fvs id2 "h" fun_num branch_trace), 
        Gt(make_bound_exp fvs id2 "l" fun_num branch_trace, Add(make_bound_exp fvs id1 "h" fun_num branch_trace, sl))));
     Geq(make_pre_own_var id2 fun_num branch_trace, make_own_var id1 fun_num branch_trace);
     Geq(make_pre_own_var id2 fun_num branch_trace, make_own_var id2 fun_num branch_trace);
     Leq(make_pre_bound_exp fvs id2 "l" fun_num branch_trace, Add(make_bound_exp fvs id1 "l" fun_num branch_trace, sl));
     Leq(make_pre_bound_exp fvs id2 "l" fun_num branch_trace, make_bound_exp fvs id2 "l" fun_num branch_trace);
     Geq(make_pre_bound_exp fvs id2 "h" fun_num branch_trace, Add(make_bound_exp fvs id1 "h" fun_num branch_trace, sl));
     Geq(make_pre_bound_exp fvs id2 "h" fun_num branch_trace, make_bound_exp fvs id2 "h" fun_num branch_trace)]  
  (* | CLetSubPtr (id1,id2,e,l) -> (*let x = y - num in ...*)
    (*let x = y + num in ... の+-を入れ替えた
    ここもマージできる*)
    new_id id1 l branch_trace; new_id id2 l branch_trace;
    let sl = exp_to_smtlib e in
    [Or(Geq(id2_pre_own, Add(id1_post_own, id2_post_own)),
     Or(Gt(Sub(make_bound_exp fvs id1 fun_num branch_trace, sl), make_bound_exp fvs id2 fun_num branch_trace), 
        Gt(make_bound_exp fvs id2 fun_num branch_trace, Sub(make_bound_exp fvs id1 fun_num branch_trace, sl))));
     Geq(id2_pre_own, id1_post_own);
     Geq(id2_pre_own, id2_post_own);
     Leq(id2_pre_scope_low, Sub(make_bound_exp fvs id1 fun_num branch_trace, sl));
     Leq(id2_pre_scope_low, make_bound_exp fvs id2 fun_num branch_trace);
     Geq(id2_pre_scope_high, Sub(make_bound_exp fvs id1 fun_num branch_trace, sl));
     Geq(id2_pre_scope_high, make_bound_exp fvs id2 fun_num branch_trace)]   *)
  | CMkArray (id,e, simpleTy,pos) -> 
    let upper_bound =
      match e with
      | ILit i -> Id (string_of_int (i-1))
      | Var id -> FV(id)
      | _ -> raise ConstrError in
    (* let x = alloc i in ... *)
    (* xに対応するvar_locationsを追加 *)
    new_id id pos branch_trace;
    (* 
    xの所有権は1;
    xの所有範囲の下限は0;
    xの所有範囲の上限はe-1 *)
    [Eq(make_own_var id fun_num branch_trace, Id "1"); 
     Eq(make_bound_exp fvs id "l" fun_num branch_trace, Id "0"); 
     Eq(make_bound_exp fvs id "h" fun_num branch_trace, upper_bound)]
  | CAssignInt (id,_) -> (* Corresponds to the example on p.14 *)
    (* x := num; ... *)
    (* 
    xの所有権は1;
    xの所有範囲の下限は0以下;
    xの所有範囲の上限は0以上 *)
    [Eq(make_own_var id fun_num branch_trace, Id "1");
     Leq(make_bound_exp fvs id "l" fun_num branch_trace, Id "0"); 
     Geq(make_bound_exp fvs id "h" fun_num branch_trace, Id "0")]
  | CAliasAddPtr (id1,id2,e,l) -> (* alias(x = y + num); ... *)
     (* x,yに対応するvar_locationsを追加 *)
    new_id id1 l branch_trace; new_id id2 l branch_trace;
      (* numをsmtlibの制約の形に変形 *)
    let sl = exp_to_smtlib e in
    (* 式評価前後のid1, id2の所有権の値と所有範囲 *)
    let id1_pre_own = make_pre_own_var id1 fun_num branch_trace in
    let id1_pre_scope_low = Add(make_pre_bound_exp fvs id1 "l" fun_num branch_trace, sl) in
    let id1_pre_scope_high = Add(make_pre_bound_exp fvs id1 "h" fun_num branch_trace, sl) in
    let id2_pre_own = make_pre_own_var id2 fun_num branch_trace in
    let id2_pre_scope_low = make_pre_bound_exp fvs id2 "l" fun_num branch_trace in
    let id2_pre_scope_high = make_pre_bound_exp fvs id2 "h" fun_num branch_trace in
    let id1_post_own = make_own_var id1 fun_num branch_trace in
    let id1_post_scope_low = Add(make_bound_exp fvs id1 "l" fun_num branch_trace, sl) in
    let id1_post_scope_high = Add(make_bound_exp fvs id1 "h" fun_num branch_trace, sl) in
    let id2_post_own = make_own_var id2 fun_num branch_trace in
    let id2_post_scope_low = make_bound_exp fvs id2 "l" fun_num branch_trace in
    let id2_post_scope_high = make_bound_exp fvs id2 "h" fun_num branch_trace in
    (* 所有範囲が隣接しているという制約 *)
    let adjacent_id1_pre_hi_id2_pre_lo = make_adjacent_scope_smtlib id1_pre_scope_high id2_pre_scope_low in
    let adjacent_id2_pre_hi_id1_pre_lo = make_adjacent_scope_smtlib id2_pre_scope_high id1_pre_scope_low in
    let adjacent_id1_post_hi_id2_post_lo = make_adjacent_scope_smtlib id1_post_scope_high id2_post_scope_low in
    let adjacent_id2_post_hi_id1_post_lo = make_adjacent_scope_smtlib id2_post_scope_high id1_post_scope_low in
    (* 
    -----   -----
    | x |   | x |
    |---| ->|---|
    | y |   | y |
    -----   -----
    (評価前のxとyの所有権の和と評価後のxとyの所有権の和は等しい　かつ
    　　評価前のxの所有範囲の下限+numと評価前のyの所有範囲の下限が等しい　かつ
    　　評価前のxの所有範囲の下限+numと評価後のxの所有範囲の下限+numが等しい　かつ
    　　評価前のyの所有範囲の下限と評価後のyの所有範囲の下限が等しい　かつ
    　　評価前のxの所有範囲の上限+numと評価前のyの所有範囲の上限が等しい　かつ
    　　評価前のxの所有範囲の上限+numと評価後のxの所有範囲の上限+numが等しい　かつ
    　　評価前のyの所有範囲の上限と評価後のyの所有範囲の上限が等しい) *)
    let smtlib1 = 
      And(Eq(Add(id1_pre_own, id2_pre_own), Add(id1_post_own, id2_post_own)),
      And(make_same_scope_smtlib id1_pre_scope_low id1_pre_scope_high id2_pre_scope_low id2_pre_scope_high,
      And(make_same_scope_smtlib id1_pre_scope_low id1_pre_scope_high id1_post_scope_low id1_post_scope_high,
        make_same_scope_smtlib id2_pre_scope_low id2_pre_scope_high id2_post_scope_low id2_post_scope_high))) in
    (* 
    -----   
    | x |   ---------    ---------
    |---| ->| x | y | or | y | x |
    | y |   ---------    ---------
    -----   
    または
    　　(評価前のxとyの所有権の和と評価後のxの所有権が等しい　かつ
    　　評価前のxとyの所有権の和と評価後のyの所有権が等しい　かつ
    　　評価前のxの所有範囲の下限+numと評価前のyの所有範囲の下限が等しい　かつ
    　　評価前のxの所有範囲の上限+numと評価前のyの所有範囲の上限が等しい　かつ
    　　　　((評価前のxの所有範囲の下限+numと評価後のxの所有範囲の下限+numが等しい　かつ
    　　　　評価前のyの所有範囲の上限と評価後のyの所有範囲の上限が等しい かつ
    　　　　評価後のxの所有範囲の上限+num+1と評価後のyの添え字の下限が等しい)
    　　　または
    　　　　(評価前のxの所有範囲の下限+numと評価後のyの所有範囲の下限が等しい　かつ
    　　　　評価前のyの所有範囲の上限と評価後のxの所有範囲の上限+numが等しい　かつ
    　　　　評価後のyの所有範囲の上限+1と評価後のxの所有範囲の下限+numが等しい))
    　　) *)
    let smtlib2 = 
      And(Eq(Add(id1_pre_own, id2_pre_own), id1_post_own),
      And(Eq(Add(id1_pre_own, id2_pre_own), id2_post_own),
      And(make_same_scope_smtlib id1_pre_scope_low id1_pre_scope_high id2_pre_scope_low id2_pre_scope_high,
      Or(And(make_same_scope_smtlib id1_pre_scope_low id2_pre_scope_high id1_post_scope_low id2_post_scope_high,
           adjacent_id1_post_hi_id2_post_lo),
         And(make_same_scope_smtlib id1_pre_scope_low id2_pre_scope_high id2_post_scope_low id1_post_scope_high,
           adjacent_id2_post_hi_id1_post_lo ))))) in
(* 
                              -----
    ---------    ---------    | x |
    | x | y | or | y | x | -> |---|
    ---------    ---------    | y |
                              -----

    または
    　　(評価前のxの所有権が評価後のxとyの所有権の和に等しい　かつ
    　　評価前のyの所有権が評価後のxとyの所有権の和に等しい　かつ
    　　評価後のxの所有範囲の下限+numと評価後のyの所有範囲の下限が等しい　かつ
    　　評価後のxの所有範囲の上限+numと評価後のyの所有範囲の上限が等しい　かつ
    　　　　((評価前のxの所有範囲の下限+numと評価後のxの所有範囲の下限+numが等しい　かつ
    　　　　評価前のyの所有範囲の上限と評価後のyの所有範囲の上限が等しい　かつ
    　　　　評価前のxの所有範囲の上限+num+1と評価前のyの所有範囲の下限が等しい)
    　　　または
    　　　　(評価前のyの所有範囲の下限と評価後のxの所有範囲の下限+numが等しい　かつ
    　　　　評価前のxの所有範囲の上限+numと評価後のyの所有範囲の上限が等しい　かつ
    　　　　評価前のyの所有範囲の上限+1と評価前のxの所有範囲の下限+numが等しい))
        ) *)
    let smtlib3 = 
      And(Eq(id1_pre_own, Add(id1_post_own, id2_post_own)),
      And(Eq(id2_pre_own, Add(id1_post_own, id2_post_own)),
      And(make_same_scope_smtlib id1_post_scope_low id1_post_scope_high id2_post_scope_low id2_post_scope_high,
        Or(And(make_same_scope_smtlib id1_pre_scope_low id2_pre_scope_high id1_post_scope_low id2_post_scope_high,
              adjacent_id1_pre_hi_id2_pre_lo),
           And(make_same_scope_smtlib id2_pre_scope_low id1_pre_scope_high id1_post_scope_low id2_post_scope_high,
              adjacent_id2_pre_hi_id1_pre_lo))))) in 
    (*                             
    ---------    ---------    ---------    ---------    
    | x | y | or | y | x | -> | x | y | or | y | x |
    ---------    ---------    ---------    ---------
                              
     または
     　　(評価前のxの所有権と評価後のxの所有権が等しい　かつ
     　　評価前のyの所有権と評価後のyの所有権が等しい　かつ
        評価後のxの所有権と評価後のyの所有権が等しい　かつ
        　　((評価前のxの所有範囲の下限+numが評価後のxの所有範囲の下限+numと等しい　かつ
        　　評価前のyの所有範囲の上限が評価後のyの所有範囲の上限と等しい　かつ
        　　評価前のxの所有範囲の上限+num+1が評価前のyの所有範囲の下限と等しい　かつ
        　　評価後のxの所有範囲の上限+num+1が評価後のyの所有範囲の下限と等しい)
        　または
        　　(評価前のxの所有範囲の下限+numが評価後のyの所有範囲の下限と等しい　かつ
        　　評価前のyの所有範囲の上限が評価後のxの所有範囲の上限+numと等しい　かつ
        　　評価前のxの所有範囲の上限+num+1が評価前のyの所有範囲の下限と等しい　かつ
        　　評価後のyの所有範囲の上限+1が評価後のxの所有範囲の下限+numと等しい)
          または
          　(評価前のyの所有範囲の下限が評価後のxの所有範囲の下限+numと等しい　かつ
        　　評価前のxの所有範囲の上限+numが評価後のyの所有範囲の上限と等しい　かつ
        　　評価前のyの所有範囲の上限+1が評価前のxの所有範囲の下限+numと等しい　かつ
        　　評価後のxの所有範囲の上限+num+1が評価後のyの所有範囲の下限と等しい)
        　または
        　　(評価前のyの所有範囲の下限が評価後のyの所有範囲の下限と等しい　かつ
        　　評価前のxの所有範囲の上限+numが評価後のxの所有範囲の上限+numと等しい　かつ
        　　評価前のyの所有範囲の上限+1が評価前のxの所有範囲の下限+numと等しい　かつ
        　　評価後のyの所有範囲の上限+1が評価後のxの所有範囲の下限+numと等しい))
        );　 *)
    let smtlib4 = 
    And(Eq(id1_pre_own, id1_post_own),
    And(Eq(id2_pre_own, id2_post_own),
    And(Eq(id1_post_own, id2_post_own),
        Or(And(make_same_scope_smtlib id1_pre_scope_low id2_pre_scope_high id1_post_scope_low id2_post_scope_high,
           And(adjacent_id1_pre_hi_id2_pre_lo,
              adjacent_id1_post_hi_id2_post_lo)),
        Or(And(make_same_scope_smtlib id1_pre_scope_low id2_pre_scope_high id2_post_scope_low id1_post_scope_high,
           And(adjacent_id1_pre_hi_id2_pre_lo,
              adjacent_id2_post_hi_id1_post_lo)),
        Or(And(make_same_scope_smtlib id2_pre_scope_low id1_pre_scope_high id1_post_scope_low id2_post_scope_high,
           And(adjacent_id2_pre_hi_id1_pre_lo,
              adjacent_id1_post_hi_id2_post_lo)),
           And(make_same_scope_smtlib id2_pre_scope_low id1_pre_scope_high id2_post_scope_low id1_post_scope_high,
           And(adjacent_id2_pre_hi_id1_pre_lo,
              adjacent_id2_post_hi_id1_post_lo)))))))) in
    [Or(smtlib1,
     Or(smtlib2,
     Or(smtlib3,
        smtlib4)))]
    (* 評価後のxの所有範囲の下限が評価後のxの所有範囲の上限以下;
      評価後のyの所有範囲の下限が評価後のyの所有範囲の上限以下 *)
     @ [Leq(id1_post_scope_low, id1_post_scope_high); Leq(id2_post_scope_low, id2_post_scope_high)]
  | CDeref (id,_) -> 
    (* let x = *y in ...
    評価後のyの所有権は0より大きい
    評価後のyの所有範囲の下限は0以下
    評価後のyの所有範囲の上限は0以上 *)
    [Gt(make_own_var id fun_num branch_trace, Id "0");
     Leq(make_bound_exp fvs id "l" fun_num branch_trace, Id "0"); 
     Geq(make_bound_exp fvs id "h" fun_num branch_trace, Id "0")]
  | CApp (id_fn,args,pos) -> 
    (* f(y1, y2, ..., yn) *)
    (* 整数引数の変数名と実引数の式の組を返す
    それ以外は何も返さない *)
    let find_subst param arg = 
      match param, arg with
      | (id, FTInt _), AExp e -> [(id, e)]
      | (_, FTRef _), AId _ -> []
      | _ -> raise ConstrError
    in
    (* 評価前の引数の型:ftid_ftsと評価後の引数の型:ftid_fts2を抽出 *)
    let (params_before_eval, params_after_eval) = lookup id_fn !fn_env in
    (* 仮引数名と整数引数に渡された式のリスト *)
    let subst = List.concat (List.map2 find_subst params_before_eval args) in
    (* 関数定義の制約生成 *)
    let subst_param_before_eval param arg = 
      match param, arg with
      (* x | () ref *)
      | (id_param, FTRef (_,ENull,ENull,_)), AId id ->
        (* 関数が定義された順番 *)
        let num = lookup id_fn funnames_numberings in
        (* 呼び出された関数の整数変数名と呼び出した関数の整数自由変数の和集合 *)
        let fvs' = union_list (List.map fst subst) fvs in
        (* 所有範囲の下限を表すデータ構造 *)
        let sll = make_bound_exp_be fvs' "l" id_param num "b" in
        (* 所有範囲の上限を表すデータ構造 *)
        let slh = make_bound_exp_be fvs' "h" id_param num "b" in
        (* 引数xの関数開始時の所有権は0　または
        　　　　(実引数の所有権が関数開始時に必要な所有権以上　かつ
        　　　　実引数の所有範囲の下限が関数開始時に必要な所有範囲の下限以下　かつ
        　　　　実引数の所有範囲の上限が関数開始時に必要な所有範囲の上限以上)　 *)
        [Or(Eq(Id "0.", make_own_var_be id_param num "b"), 
         And(Geq(make_own_var id fun_num branch_trace, make_own_var_be id_param num "b"),
         And(Leq(make_bound_exp fvs id "l" fun_num branch_trace, smtlib_subst subst sll),
             Geq(make_bound_exp fvs id "h" fun_num branch_trace, smtlib_subst subst slh))))]
         (* x | () ref (left, right, ownership) *)
      | (_, FTRef (_,el,eh,f)), AId id -> 
        (* 引数の篩型中の整数変数を別の式で置き換え *)
        let scope_low = exp_to_smtlib (exp_subst subst el) in
        let scope_high = exp_to_smtlib (exp_subst subst eh) in
        (* プログラマ指定の所有権が0　または
        　　　　(実引数の所有権がプログラマ指定の所有権以上　かつ
        　　　　実引数の所有範囲の下限がプログラマ指定の所有範囲の下限以下　かつ
        　　　　実引数の所有範囲の上限がプログラマ指定の所有範囲の上限以上) *)
        [Or(Eq(Id "0.", Id (string_of_float f)),
         And(Geq(make_own_var id fun_num branch_trace, Id (string_of_float f)),
         And(Leq(make_bound_exp fvs id "l" fun_num branch_trace, scope_low),
             Geq(make_bound_exp fvs id "h" fun_num branch_trace, scope_high))))]
        (* 整数の時は何もしない *)
      | _, AExp _ -> []
      | _ -> raise ConstrError
    in
    (* 関数引数に対するsmtlibの条件式の生成 *)
    let constraints1 = List.concat (List.map2 subst_param_before_eval params_before_eval args) in
    (* g1とほぼ同様,呼び出した関数の評価終了時の制約 *)
    let subst_param_after_eval ftid_ft arg = 
      match ftid_ft, arg with
      | (id_param, FTRef (_,ENull,ENull,_)), AId id ->
        let num = lookup id_fn funnames_numberings in
        let fvs' = union_list (List.map fst subst) fvs in
        let sll = make_bound_exp_be fvs' "l" id_param num "e" in
        let slh = make_bound_exp_be fvs' "h" id_param num "e" in
        (* 引数のvar_locationsを生成 *)
        new_id id pos branch_trace;
        [Eq(make_own_var id fun_num branch_trace, make_own_var_be id_param num "e");
         Eq(make_bound_exp fvs id "l" fun_num branch_trace, smtlib_subst subst sll);
         Eq(make_bound_exp fvs id "h" fun_num branch_trace, smtlib_subst subst slh)]
      | (_, FTRef (_,el,eh,f)), AId id -> 
        let l_arg_exp = exp_subst subst el in
        let h_arg_exp = exp_subst subst eh in
        (* 引数のvar_locationsを生成 *)
        new_id id pos branch_trace;
        [Eq(make_own_var id fun_num branch_trace, Id (string_of_float f));
         Eq(make_bound_exp fvs id "l" fun_num branch_trace, exp_to_smtlib l_arg_exp);
         Eq(make_bound_exp fvs id "h" fun_num branch_trace, exp_to_smtlib h_arg_exp)]
      | _, AExp _ -> []
      | _ -> raise ConstrError
    in
    (* 関数評価後に対するsmtlibの条件式の生成 *)
    let constraints2 = List.concat (List.map2 subst_param_after_eval params_after_eval args) in
    (* 関数評価前と評価後の条件式を結合 *)
    constraints1 @ constraints2
    | _ -> raise ConstrError
    (*追加分，あとで消す*)
  
(* 関数仮引数のうち整数引数名を返す *)
let find_intv param = 
  match param with
  | (id, FTInt _) -> [id]
  | _ -> []

(* 関数仮引数のうち参照型である場合はその引数名を返す *)
let find_ref_id param = 
  match param with
  | (id, FTRef _) -> [id]
  | _ -> [] 

(* 関数仮引数のうち参照型である引数の所有権の加減，添え字の上限，所有権の組を返す
こんなに周りくどいやり方する必要ある？？？？ *)
let rec assoc_ft ref_id params = 
  match params with
  | (id, FTRef (_,el,eh,f)) :: _ when id = ref_id -> (el,eh,f)
  | _ :: params' -> assoc_ft ref_id params'
  | [] -> raise Not_found

