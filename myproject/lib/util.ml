open Syntax
open SimpleTyping
open Printf
open SmtlibSyntax

exception ElimError
exception Error of string
let err s = raise (Error s)

(* 環境からxに該当するものを探す *)
let rec lookup x env =
  try List.assoc x env with Not_found -> err ("variable not bound: " ^ x)

(* 
env: 式内で定義された整数変数名と式の組
fun_name: 関数名
args: 引数名のリスト
exp: 関数の式
式中に現れる整数変数を束縛された時点での具体的な式で置き換える 
*)
let rec elim_int_var env fun_name args exp = 
  match exp with
  | LetIntExp (id,exp1,exp2) -> 
    LetIntExp(id, elim_int_var env fun_name args exp1, elim_int_var env fun_name args exp2)
  | LetAddPtrExp(id,id2,exp1,exp2) ->
    LetAddPtrExp(id, id2, elim_int_var env fun_name args exp1, elim_int_var env fun_name args exp2)
  | LetDerefExp(id1,id2,e) ->
    LetDerefExp(id1, id2, elim_int_var env fun_name args e)
  | IfExp (exp1,exp2,exp3) ->
    IfExp(elim_int_var env fun_name args exp1, elim_int_var env fun_name args exp2, elim_int_var env fun_name args exp3)
  | IfnpExp (id,exp1,exp2) ->
    IfnpExp(id, elim_int_var env fun_name args exp1, elim_int_var env fun_name args exp2)
  | LetAllocExp (id,exp1,simpleTy,exp2) ->
    LetAllocExp(id, elim_int_var env fun_name args exp1, simpleTy, elim_int_var env fun_name args exp2)
  | Assign (id1,exp1,exp2) ->
    Assign(id1, elim_int_var env fun_name args exp1, elim_int_var env fun_name args exp2)
  | AssignInt (id,exp1,exp2) ->
    AssignInt(id, elim_int_var env fun_name args exp1, elim_int_var env fun_name args exp2)
  | AssignPtr (id1,id2,e) ->
    AssignPtr(id1, id2, elim_int_var env fun_name args e)
  | Alias (exp1,exp2,exp3) ->
    Alias(elim_int_var env fun_name args exp1, elim_int_var env fun_name args exp2, elim_int_var env fun_name args exp3) 
  | AliasDeref(id1,id2,e) ->
    AliasDeref(id1, id2, elim_int_var env fun_name args e)
  | AliasAddPtr (id1,id2,i,e) ->
    AliasAddPtr(id1, id2, i, elim_int_var env fun_name args e)
  | Assert (exp1,exp2) ->
    Assert(elim_int_var env fun_name args exp1, elim_int_var env fun_name args exp2)
  | Seq (exp1,exp2) ->
    Seq(elim_int_var env fun_name args exp1, elim_int_var env fun_name args exp2)
  | AppExp (id,es) ->
    AppExp(id, List.map (elim_int_var env fun_name args) es)
  | EqExp (exp1,exp2) ->
    EqExp(elim_int_var env fun_name args exp1, elim_int_var env fun_name args exp2)
  | LtExp (exp1, exp2) ->
    LtExp(elim_int_var env fun_name args exp1, elim_int_var env fun_name args exp2)
  | GtExp (exp1, exp2) ->
    GtExp(elim_int_var env fun_name args exp1, elim_int_var env fun_name args exp2)
  | LeqExp (exp1, exp2) ->
    LeqExp(elim_int_var env fun_name args exp1, elim_int_var env fun_name args exp2)
  | GeqExp (exp1, exp2) ->
    GeqExp(elim_int_var env fun_name args exp1, elim_int_var env fun_name args exp2)
  | NeqExp (exp1, exp2) ->
    NeqExp(elim_int_var env fun_name args exp1, elim_int_var env fun_name args exp2)
  | AndExp (exp1,exp2) ->
    AndExp(elim_int_var env fun_name args exp1, elim_int_var env fun_name args exp2)
  | OrExp (exp1,exp2) ->
    OrExp(elim_int_var env fun_name args exp1, elim_int_var env fun_name args exp2)
  | NotExp e ->
    NotExp (elim_int_var env fun_name args e)
  | PlusExp (exp1,exp2) -> 
    PlusExp(elim_int_var env fun_name args exp1, elim_int_var env fun_name args exp2)
  | MinusExp (exp1,exp2) -> 
    MinusExp(elim_int_var env fun_name args exp1, elim_int_var env fun_name args exp2)
  | MultExp (exp1,exp2) -> 
    MultExp(elim_int_var env fun_name args exp1, elim_int_var env fun_name args exp2)
  | Var x -> 
    (* 変数が引数由来の場合は具体化しない *)
    if List.mem x args then 
      Var x
    (* 変数が関数内で定義された整数変数の場合は具体化する *)
    else if lookup x (lookup fun_name !all_tyenv) = SInt then
      lookup x env
    (* それ以外の場合は具体化しない *)
    else 
      Var x
  | _ -> exp

(* if節の分岐を表す構造体 *)
type branch =
  | Then
  | Else

(* branchのフォーマッター *)
let pp_branch fmt branch =
  match branch with
  | Then -> Format.fprintf fmt "then"
  | Else -> Format.fprintf fmt "else"

(* branch listのフォーマッター *)
let rec pp_branch_trace fmt branch_trace =
  match branch_trace with
  | [] -> Format.fprintf fmt ""
  | branch :: branch_trace' -> Format.fprintf fmt "_%a%a" pp_branch branch pp_branch_trace branch_trace'

(* branch listの文字列化 *)
let rec branch_trace_to_str branch_trace = 
  match branch_trace with
  | [] -> ""
  | Then :: branch_trace' -> sprintf "_then%s" (branch_trace_to_str branch_trace')
  | Else :: branch_trace' -> sprintf "_else%s" (branch_trace_to_str branch_trace')

(* プログラムの制約式をsmtlibの読める制約の形に直す *)
let rec exp_to_smtlib exp = 
  match exp with 
  | EqExp (e1,e2) ->
    let s1 = exp_to_smtlib e1 in
    let s2 = exp_to_smtlib e2 in
    Eq(s1, s2)
  | LtExp (e1, e2) ->
    let s1 = exp_to_smtlib e1 in
    let s2 = exp_to_smtlib e2 in
    Lt(s1, s2)
  | GtExp (e1, e2) ->
    let s1 = exp_to_smtlib e1 in
    let s2 = exp_to_smtlib e2 in
    Gt(s1, s2)
  | LeqExp (e1, e2) ->
    let s1 = exp_to_smtlib e1 in
    let s2 = exp_to_smtlib e2 in
    Leq(s1, s2)
  | GeqExp (e1, e2) ->
    let s1 = exp_to_smtlib e1 in
    let s2 = exp_to_smtlib e2 in
    Geq(s1, s2)
  | NeqExp (e1, e2) ->
    let s1 = exp_to_smtlib e1 in
    let s2 = exp_to_smtlib e2 in
    Not(Eq(s1, s2))
  | AndExp (e1,e2) ->
    let s1 = exp_to_smtlib e1 in
    let s2 = exp_to_smtlib e2 in
    And(s1, s2)
  | OrExp (e1,e2) ->
    let s1 = exp_to_smtlib e1 in
    let s2 = exp_to_smtlib e2 in
    Or(s1, s2)
  | NotExp e ->
    let s = exp_to_smtlib e in
    Not s
  | PlusExp (e1,e2) -> 
    let s1 = exp_to_smtlib e1 in
    let s2 = exp_to_smtlib e2 in
    Add(s1, s2)
  | MinusExp (e1,e2) -> 
    let s1 = exp_to_smtlib e1 in
    let s2 = exp_to_smtlib e2 in
    Sub(s1, s2)
  | MultExp (e1,e2) -> 
    let s1 = exp_to_smtlib e1 in
    let s2 = exp_to_smtlib e2 in
    Mul(s1, s2)
  (* | EDiv (e1,e2) -> 
    let s1 = exp_to_smtlib e1 in
    let s2 = exp_to_smtlib e2 in
    Div(s1, s2) *)
  | ILit i ->
    if i >= 0 then
      Id (string_of_int i)
    else 
      Id (sprintf "(%d)" (-i))
  | Var x -> FV x
  | _ -> raise ElimError

(* リストls1とls2を重複を除いて結合する *)
let rec union_list ls1 ls2 = 
  match ls1 with
  | [] -> ls2
  | x :: ls1' -> if List.mem x ls2 then union_list ls1' ls2 else union_list ls1' (x :: ls2)


(* st中の変数をsubstに従って別の制約式に置き換える *)
let rec smtlib_subst subst st = 
  match st with
  | Or (st1,st2) ->
    Or(smtlib_subst subst st1, smtlib_subst subst st2)
  | And (st1,st2) ->
    And(smtlib_subst subst st1, smtlib_subst subst st2)
  | Imply (st1,st2) ->
    Imply(smtlib_subst subst st1, smtlib_subst subst st2)
  | Not st ->
    Not (smtlib_subst subst st)
  | Eq (st1,st2) ->
    Eq(smtlib_subst subst st1, smtlib_subst subst st2)
  | Lt (st1,st2) ->
    Lt(smtlib_subst subst st1, smtlib_subst subst st2)
  | Gt (st1,st2) ->
    Gt(smtlib_subst subst st1, smtlib_subst subst st2)
  | Leq (st1,st2) ->
    Leq(smtlib_subst subst st1, smtlib_subst subst st2)
  | Geq (st1,st2) ->
    Geq(smtlib_subst subst st1, smtlib_subst subst st2)
  | Add (st1,st2) ->
    Add(smtlib_subst subst st1, smtlib_subst subst st2)
  | Sub (st1,st2) ->
    Sub(smtlib_subst subst st1, smtlib_subst subst st2)
  | Mul (st1,st2) ->
    Mul(smtlib_subst subst st1, smtlib_subst subst st2)
  | Div (st1,st2) ->
    Div(smtlib_subst subst st1, smtlib_subst subst st2)
  | FV x -> 
    (try 
       exp_to_smtlib (lookup x subst)
     with
       Error _ -> st)
  | _ -> st
