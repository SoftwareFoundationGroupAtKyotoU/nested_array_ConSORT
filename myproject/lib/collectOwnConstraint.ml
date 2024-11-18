(** Translator from Syntax.Exp to OwnConstraintSyntax.constr *)
open Syntax
open SimpleTyping
open OwnConstraintSyntax
open Util

exception ConstrError

(* (関数名 * ((引数名*引数の型)のリスト　*　(評価後の引数名*引数の型)のリスト))のリスト *)
let fn_env : (id * ((id * ftype) list * (id * ftype) list)) list ref = ref []

let rec collect_exp env fun_name args position exp =
  match exp with
  | IfExp (exp1,exp2,exp3) ->
    let c1 = collect_exp env fun_name args position exp1 in 
    let c2 = collect_exp env fun_name args (position+1) exp2 in
    let c3 = collect_exp env fun_name args (position+1) exp3 in
    (* exp1中の整数変数を式に置き換え *)
    let exp1' = elim_int_var env fun_name args exp1 in
    CIf(exp1', c2, c3, position) :: c1
  | LetIntExp (id,exp1,exp2) ->
    let exp1' = elim_int_var env fun_name args exp1 in
    let env' = (id, exp1') :: env in
    let c1 = collect_exp env' fun_name args position exp1' in
    let c2 = collect_exp env' fun_name args (position+1) exp2 in
    c1 @ c2
  | LetDerefExp (id1,id2,exp) ->
    let c = collect_exp env fun_name args (position+1) exp in
    CLetDeref(id1, id2, position) :: c
  | LetAddPtrExp (id1,id2,exp1,exp2) ->
    let c1 = collect_exp env fun_name args position exp1 in
    let c2 = collect_exp env fun_name args (position+1) exp2 in
    CLetAddPtr(id1, id2, elim_int_var env fun_name args exp1, position) :: c1 @ c2
  (* | ELetSubPtr (id1,id2,exp1,exp2) ->
    let c1 = collect_exp env fun_name args position exp1 in
    let c2 = collect_exp env fun_name args (position+1) exp2 in
    CLetSubPtr(id1, id2, elim_v env fun_name args exp1, l) :: c1 @ c2 *)
  | LetAllocExp (id,exp1,simpleTy,exp2) ->
    let c = collect_exp env fun_name args (position+1) exp2 in
    CMkArray(id, exp1, simpleTy, position) :: c
  | AssignInt (id,exp1,exp2) ->
    let c1 = collect_exp env fun_name args position exp1 in
    let c2 = collect_exp env fun_name args (position+1) exp2 in
    CAssignInt(id, position) :: c1 @ c2
  | AssignPtr (id1,id2,esp) ->
    let c = collect_exp env fun_name args (position+1) esp in
    CAssignRef(id1, id2, position) :: c
  | AliasDeref (id1,id2,e) -> 
    let c = collect_exp env fun_name args (position+1) e in
    CAliasDeref(id1, id2, position) :: c
  | AliasAddPtr (id1,id2,exp1,exp2) -> 
    let c1 = collect_exp env fun_name args position exp1 in
    let c2 = collect_exp env fun_name args (position+1) exp2 in
    CAliasAddPtr(id1, id2, elim_int_var env fun_name args exp1, position) :: c1 @ c2
  | Assert (exp1,exp2) ->
    let c1 = collect_exp env fun_name args position exp1 in
    let c2 = collect_exp env fun_name args (position+1) exp2 in
    c1 @ c2
  | Seq (exp1,exp2) ->
    let c1 = collect_exp env fun_name args position exp1 in
    let c2 = collect_exp env fun_name args (position+1) exp2 in
    c1 @ c2
  | Deref id ->
    [CDeref(id, position)]
  | AppExp (id,es) ->
    let cs = List.concat (List.map (collect_exp env fun_name args position) es) in
    (match lookup id (lookup "main" !all_tyenv) with 
    | SFun (simple_arg_types, _) ->
    (* 関数の実引数を変換　参照型はAid (変数名)，整数型はAExp (束縛された値で置き換え) *)
      let convert_args param arg =
        match param, arg with
        | SRef _, Var id -> AId id
        | SInt, _ -> AExp (elim_int_var env fun_name args arg) 
        (* argsは現在制約作成中の関数の仮引数名，argは既に制約作成の終わった関数の実引数名 *)
        | _ -> raise ConstrError
      in
      let converted_args = List.map2 convert_args simple_arg_types es in
      CApp(id, converted_args, position) :: cs
    | _ -> raise ConstrError)
  | _ -> []


let collect_function_own_constraints fdef =
  let (id, _, annotation, e) = fdef in
  let (args_before_eval, args_after_eval, _) = annotation in
  let args = List.map fst args_before_eval in 
  fn_env := (id, (args_before_eval, args_after_eval)) :: !fn_env;
  (id, collect_exp [] id args 1 e)

let collect_program_own_constraints prog = 
  let (fdefs, exp) = prog in
  let fun_constraints = List.map collect_function_own_constraints fdefs in
  fn_env := ("main", ([], [])) :: !fn_env;
  let main_constraints = ("main", collect_exp [] "main" [] 1 exp) in
  let all_cs = main_constraints :: fun_constraints in
  all_cs