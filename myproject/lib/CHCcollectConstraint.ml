(** Translator from Syntax.exp to Syntax.chc *)

open Syntax
open CHCSyntax
open Util

exception ConstrError

(* (関数名，(関数型環境，評価前型，評価後型，返り値型の組))のリスト *)
let fn_env_chc : (id * ((ftype_id * ftype) list * (ftype_id * ftype) list * ftype)) list ref = ref []

(* プログラムをCHCの制約を表すデータ型に変換 *)
let rec chc_collect_exp pos exp =
  match exp with
  | IfExp (e1,e2,e3) ->
    let c1 = chc_collect_exp pos e1 in 
    let c2 = chc_collect_exp (pos+1) e2 in
    let c3 = chc_collect_exp (pos+1) e3 in
    CHCIf(e1, c2, c3, pos) :: c1
  | LetIntExp (id,e1,e2) ->
    let c2 = chc_collect_exp (pos+1) e2 in
    CHCLetInt(id, e1, pos) :: c2
  | LetDerefExp (id1,id2,e) ->
    let c = chc_collect_exp (pos+1) e in
    CHCLetDeref(id1, id2, pos) :: c
  | LetAddPtrExp (id1,id2,e1,e2) ->
    let c1 = chc_collect_exp pos e1 in
    let c2 = chc_collect_exp (pos+1) e2 in
    CHCLetAddPtr(id1, id2, e1, pos) :: c1 @ c2
  | LetAllocExp (id,e1,_,e2) ->
    let c1 = chc_collect_exp pos e1 in
    let c2 = chc_collect_exp (pos+1) e2 in
    CHCAlloc(id, e1, pos) :: c1 @ c2
  | AssignInt (id,e1,e2) ->
    let c2 = chc_collect_exp (pos+1) e2 in
    CHCAssignInt(id, e1, pos) :: c2
  | AssignPtr (id1,id2,e) ->
    let c = chc_collect_exp (pos+1) e in
    CHCAssignRef(id1, id2, pos) :: c
  | AliasDeref (id1,id2,e) -> 
    let c = chc_collect_exp (pos+1) e in
    CHCAliasDeref(id1, id2, pos) :: c
  | AliasAddPtr (id1,id2,e1,e2) -> 
    let c1 = chc_collect_exp pos e1 in
    let c2 = chc_collect_exp (pos+1) e2 in
    CHCAliasAddPtr(id1, id2, e1, pos) :: c1 @ c2
  | Assert (e1,e2) ->
    let c2 = chc_collect_exp (pos+1) e2 in
    CHCAssert(e1, pos) :: c2
  | Seq (e1,e2) ->
    let c1 = chc_collect_exp pos e1 in
    let c2 = chc_collect_exp (pos+1) e2 in
    c1 @ c2
  | AppExp (id,es) ->
    let cs = List.concat (List.map (chc_collect_exp pos) es) in
    CHCApp(id, es, pos) :: cs
  | _ -> []

let elim_hash ftid =
  match ftid with
  | RawId id -> id
  | HashId id -> id

let chc_collect_fdef fdef =
  (* 関数名，引数，評価前後の型注釈，本体式 *)
  let (id, _, ann, e) = fdef in
  (* 評価前型，評価後型，返り値型 *)
  let (ftid_fts1, ftid_fts2, ft) = ann in 
  (* 最後に評価されうる式のリスト *)
  let return_exp = ret_of_exp [] e in
  (* 関数型環境の更新 *)
  fn_env_chc := (id, (ftid_fts1, ftid_fts2, ft)) :: !fn_env_chc;
  (id, chc_collect_exp 1 e, return_exp)

let chc_collect_prog prog = 
  let (fdefs, exp) = prog in
  (* 関数名，CHCの制約を表すデータ型，最後に評価されうる式の組 *)
  let ics = List.map chc_collect_fdef fdefs in
  (* 関数環境に本体式を追加 *)
  fn_env_chc := ("main", ([], [], FTInt(Id "true"))) :: !fn_env_chc;
  let ic = ("main", chc_collect_exp 1 exp, []) in
  (* 本体式の制約を追加 *)
  let all_cs = ic :: ics in
  all_cs

