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
    

