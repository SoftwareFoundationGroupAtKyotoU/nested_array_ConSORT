(* Module for ownership constraint generation(smtlib acceptable form) *)

open OwnConstraintSyntax
open SmtlibSyntax
open Util
open Format

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

(* 所有範囲の下限を定める (cかd)_(fun_num)_l_(fv)_(id)_(pos)_(branch_trace)
  nって何，idはどの変数の所有権を計算しているか？
  fvs=["a", "b"], id="x", fun_num=1, branch_trace=[then]の場合
    Add(Mul(Id( "c_1_l_a_x_pos?_then"), FV(a) ), Add(Mul(Id("c_1_l_b_x_?_then"), FV(b)), "d_1_l_x_pos?_then") )
    つまり "c_1_l_a_x_pos?_then" * FV(a) + "c_1_l_b_x_pos?_then" * FV(b) + "d_1_l_x_pos?_then"
  *)
let rec make_low_bound_var fvs id fun_num branch_trace = 
  let id_pos = lookup_pos id branch_trace !var_locations in
  match fvs with
  | [] -> 
    let var_name = asprintf "d_%d_l_%s_%d_%a" fun_num id id_pos pp_branch_trace branch_trace in
    Id(var_name)
  | fv :: fvs' ->
    let var_name = asprintf "c_%d_l_%s_%s_%d_%a" fun_num fv id id_pos pp_branch_trace branch_trace in
    Add( Mul(Id(var_name), FV(fv)), make_low_bound_var fvs' id fun_num branch_trace)
  (* | [] -> Id("d_" ^ (string_of_int n) ^ "_l_" ^ id ^ "_" ^ (string_of_int (lookup_ifel id ifel !id_count)) ^ ifel_to_str ifel)
  | fv :: fvs' ->
    Add(Mul(Id("c_" ^ (string_of_int n) ^ "_l_" ^ fv ^ "_" ^ id ^ "_" ^ (string_of_int (lookup_ifel id ifel !id_count)) ^ ifel_to_str ifel), FV(fv)),
        lo fvs' id n ifel) *)

(* 所有範囲の上限を定める (cかd)_(fun_num)_h_(fv)_(id)_(pos)_(branch_trace)
  fvs=["a", "b"], id="x", fun_num=1, branch_trace="then"の場合
    Add(Mul(Id( "c_1_h_a_x_pos?_then"), FV(a) ), Add(Mul(Id("c_1_h_b_x_pos?_then"), FV(b)), "d_1_h_x_pos?_then") )
    つまり "c_1_h_a_x_pos?_then" * FV(a) + "c_1_h_b_x_pos?_then" * FV(b) + "d_1_h_x_pos?_then"
*)
let rec make_high_bound_var fvs id fun_num branch_trace = 
  let id_pos = lookup_pos id branch_trace !var_locations in
  match fvs with
  | [] -> 
    let var_name = asprintf "d_%d_h_%s_%d_%a" fun_num id id_pos pp_branch_trace branch_trace in
    Id(var_name)
  | fv :: fvs' ->
    let var_name = asprintf "c_%d_h_%s_%s_%d_%a" fun_num fv id id_pos pp_branch_trace branch_trace in
    Add( Mul(Id(var_name), FV(fv)), make_high_bound_var fvs' id fun_num branch_trace)


