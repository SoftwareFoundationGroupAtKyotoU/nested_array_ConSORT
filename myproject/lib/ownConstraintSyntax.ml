open Syntax
open Format


(* 関数内の位置を表す型，イメージ的にはプログラムの書かれた行 *)
type pos = int

(** AST with position information used for ownership inference *)
type constr = 
  | CIf of exp * constr list * constr list * pos
  | CIfnp of id * constr list * constr list * pos
  (* | CLet of id * id * pos *)
  | CLetDeref of id * id * pos
  | CLetAddPtr of id * id * exp * pos
  (* | CLetSubPtr of id * id * exp * pos *)
  | CMkArray of id * exp * simpleTy * pos
  | CAssignInt of id * pos
  | CAssignRef of id * id * pos
  (* | CAlias of id * id * pos *)
  | CAliasDeref of id * id * pos
  | CAliasAddPtr of id * id * exp * pos
  | CDeref of id * pos
  | CApp of id * arg list * pos
and arg = 
  | AExp of exp
  | AId of id

let rec cons_to_program constr =
  match constr with
  | CLetDeref (id1, id2, _) ->
    asprintf "  let %s = *%s in\n" id1 id2
  | CLetAddPtr (id1, id2, exp, _) ->
    let s_exp = exp_to_program exp in
    asprintf "  let %s = %s + %s in\n" id1 id2 s_exp
  | CMkArray (id, exp, simpleTy, _) ->
    let s_exp = exp_to_program exp in
    asprintf "  let %s = alloc %s : %a in\n" id s_exp pp_simpleTy simpleTy
  | CAssignInt (id, _) ->
    asprintf "  %s := (int);\n" id
  | CAssignRef (id1, id2, _) ->
    asprintf "  %s := %s;\n" id1 id2
  | CAliasDeref (id1, id2, _) ->
    asprintf "  alias(%s=*%s);\n" id1 id2
  | CAliasAddPtr (id1, id2, exp, _) ->
    let s_exp = exp_to_program exp in
    asprintf "  alias(%s=%s+%s);\n" id1 id2 s_exp
  | CDeref (id, _) ->
    asprintf "  let _ = *%s in\n" id
  | CApp (id, arg_lis, _) -> 
    let s_arg_lis = String.concat ", " (List.map arg_to_program arg_lis) in
    asprintf "  let _ = %s(%s) in\n" id s_arg_lis
  | CIf (_, c_lis1, c_lis2, _) ->
    let s1 = String.concat "" (List.map cons_to_program c_lis1) in
    let s2 = String.concat "" (List.map cons_to_program c_lis2) in
    asprintf "  if exp then {\n  %s} else {\n  %s}\n" s1 s2
  | _ -> ""
and arg_to_program arg =
  match arg with
  | AExp exp -> exp_to_program exp
  | AId id -> id
and exp_to_program exp =
  match exp with
  | Var id -> id
  | ILit num -> string_of_int num
  | LtExp (e1, e2) ->
    let s1 = exp_to_program e1 in
    let s2 = exp_to_program e2 in
    asprintf "%s < %s" s1 s2
  | LeqExp (e1, e2) ->
    let s1 = exp_to_program e1 in
    let s2 = exp_to_program e2 in
    asprintf "%s <= %s" s1 s2
  | _ -> "not yet implemented"
