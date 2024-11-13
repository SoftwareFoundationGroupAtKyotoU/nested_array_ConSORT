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
