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

(* プログラムの構文木の出力 *)
let rec print_exp exp =
  match exp with
  | Let (id,e1,e2) ->
    (print_string ("Let( \"" ^ id ^ "\", ");
      print_exp e1;
      print_string ", ";
      print_exp e2;
      print_string ")")
  | LetIntExp (id,e1,e2) ->
    (print_string ("LetIntExp( \"" ^ id ^ "\", ");
      print_exp e1;
      print_string ", ";
      print_exp e2;
      print_string ")")
  (* | LetVarPtr (id1,id2,e) ->
    (print_string ("ELetVarPtr(" ^ id1 ^ ", ");
      print_string id2;
      print_string ", ";
      print_exp e;
      print_string ")") *)
  | LetDerefExp (id1,id2,e) ->
    (print_string ("LetDerefExp( \""^ id1 ^ "\", ");
      print_string id2;
      print_string ", ";
      print_exp e;
      print_string ")")
  | LetAddPtrExp (id1,id2,e1,e2) ->
    (print_string ("LetAddPtrExp( \"" ^ id1 ^ "\", ");
      print_string id2;
      print_string ", ";
      print_exp e1;
      print_string ", ";
      print_exp e2;
      print_string ")")
  (* | ELetSubPtr (id1,id2,e1,e2) ->
    (print_string ("ELetSubPtr(" ^ id1 ^ ", ");
      print_string id2;
      print_string ", ";
      print_exp e1;
      print_string ", ";
      print_exp e2;
      print_string ")") *)
  | IfExp (e1,e2,e3) ->
    (print_string "IfExp(";
      print_exp e1;
      print_string ", "; 
      print_exp e2;
      print_string ", ";
      print_exp e3;
      print_string ")")
  | IfnpExp (id,e1,e2) ->
    (print_string "IfnpExp( \"";
      print_string id;
      print_string "\", "; 
      print_exp e1;
      print_string ", ";
      print_exp e2;
      print_string ")")
  | LetAllocExp (id,e1,simpleTy,e2) ->
    (print_string ("LetAllocExp( \"" ^ id ^ "\", ");
      print_exp e1;
      print_string ", ";
      print_simplety simpleTy;
      print_string ", ";
      print_exp e2;
      print_string ")")
  | Assign (id1,e1,e2) ->
    (print_string ("Assign( \"" ^ id1 ^ "\", ");
      print_exp e1;
      print_string ", ";
      print_exp e2;
      print_string ")")
  | AssignInt (id,e1,e2) ->
    (print_string ("AssignInt( \"" ^ id ^ "\", ");
      print_exp e1;
      print_string ", ";
      print_exp e2;
      print_string ")")
  | AssignPtr (id1,id2,e) ->
    (print_string ("AssignPtr( \"" ^ id1 ^ "\", \"");
      print_string id2;
      print_string "\", ";
      print_exp e;
      print_string ")")
  | Alias (e1,e2,e3) ->
    (print_string "Alias(";
      print_exp e1;
      print_string ", "; 
      print_exp e2;
      print_string ", ";
      print_exp e3;
      print_string ")")
  (* | EAliasVarPtr (id1,id2,e) ->
    (print_string ("EAliasVarPtr(" ^ id1 ^ ", ");
      print_string id2;
      print_string ", ";
      print_exp e;
      print_string ")") *)
  | AliasDeref (id1,id2,e) ->
    (print_string ("AliasDeref( \"" ^ id1 ^ "\", ");
      print_string id2;
      print_string ", ";
      print_exp e;
      print_string ")")
  | AliasAddPtr (id1,id2,i,e) ->
    (print_string ("AliasAddPtr( \"" ^ id1 ^ "\", \"");
      print_string id2;
      print_string "\", ";
      print_exp i;
      print_string ", ";
      print_exp e;
      print_string ")")
  | Assert (e1,e2) ->
    (print_string "Assert(";
      print_exp e1;
      print_string ", ";
      print_exp e2;
      print_string ")")
  | Seq (e1,e2) ->
    (print_string "Seq(";
      print_exp e1;
      print_string ", ";
      print_exp e2;
      print_string ")")
  | Deref id ->
    print_string ("Deref( \"" ^ id ^ "\")")
  | AppExp (id,es) ->
    (print_string ("AppExp( \"" ^ id ^ "\", [");
      print_exps es;
      print_string "])") 
  | EqExp (e1,e2) ->
    (print_string "EqExp(";
      print_exp e1;
      print_string ", ";
      print_exp e2;
      print_string ")")
  | LtExp (e1, e2) ->
    (print_string "LtExp(";
      print_exp e1;
      print_string ", ";
      print_exp e2;
      print_string ")")
  | GtExp (e1, e2) ->
    (print_string "GtExp(";
      print_exp e1;
      print_string ", ";
      print_exp e2;
      print_string ")")
  | LeqExp (e1, e2) ->
    (print_string "LeqExp(";
      print_exp e1;
      print_string ", ";
      print_exp e2;
      print_string ")")
  | GeqExp (e1, e2) ->
    (print_string "GeqExp(";
      print_exp e1;
      print_string ", ";
      print_exp e2;
      print_string ")")
  | NeqExp (e1, e2) ->
    (print_string "NeqExp(";
      print_exp e1;
      print_string ", ";
      print_exp e2;
      print_string ")")
  | AndExp (e1,e2) ->
    (print_string "AndExp(";
      print_exp e1;
      print_string ", ";
      print_exp e2;
      print_string ")")
  | OrExp (e1,e2) ->
    (print_string "OrExp(";
      print_exp e1;
      print_string ", ";
      print_exp e2;
      print_string ")")
  | NotExp e ->
    (print_string "NotExp(";
      print_exp e;
      print_string ")")
  | PlusExp (e1,e2) -> 
    (print_string "PlusExp(";
      print_exp e1;
      print_string ", ";
      print_exp e2;
      print_string ")")
  | MinusExp (e1,e2) -> 
    (print_string "MinusExp(";
      print_exp e1;
      print_string ", ";
      print_exp e2;
      print_string ")")
  | MultExp (e1,e2) -> 
    (print_string "MultExp(";
      print_exp e1;
      print_string ", ";
      print_exp e2;
      print_string ")")
  (* | EDiv (e1,e2) -> 
    (print_string "EDiv(";
      print_exp e1;
      print_string ", ";
      print_exp e2;
      print_string ")") *)
  | Unit -> 
    print_string "Unit"
  (* | EConstFail ->
    print_string "fail" *)
  | ILit i ->
    print_string "ILit ";
    print_int i
  | BLit b ->
    print_string "BLit ";
    if b then print_string "true" else print_string "false"
  | Nondet ->
    print_string "Nondet"
  (* | EConstTrue ->
    print_string "true"
  | EConstFalse ->
    print_string "false" *)
  | Var x -> 
    print_string "Var \"";
    print_string (x ^ "\"")
  | ENull ->
    print_string "ENull"
and print_exps es =
  match es with
  | [] -> ()
  | e :: [] -> print_exp e
  | e :: es' -> print_exp e; print_string "; "; print_exps es'

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
  (* | Div (st1,st2) ->
    Div(smtlib_subst subst st1, smtlib_subst subst st2) *)
  | FV x -> 
    (try 
       exp_to_smtlib (lookup x subst)
     with
       _ -> st)
  | _ -> st


(* 式exp中の変数をsubstに従って別の式に置き換える *)
let rec exp_subst subst exp = 
  match exp with
  | LetIntExp (id,e1,e2) -> 
    LetIntExp(id, exp_subst subst e1, exp_subst subst e2)
  (* | LetVarPtr (id1,id2,e) ->
    ELetVarPtr(id1, id2, exp_subst subst e) *)
  | LetDerefExp (id1,id2,e) ->
    LetDerefExp(id1, id2, exp_subst subst e)
  | LetAddPtrExp (id1,id2,e1,e2) ->
    LetAddPtrExp(id1, id2, exp_subst subst e1, exp_subst subst e2)
  | Let _ ->
    err ("exp_subst Error: If this error occurs, the elaborate module is wrong.")
  | IfExp (e1,e2,e3) ->
    IfExp(exp_subst subst e1, exp_subst subst e2, exp_subst subst e3)
  | IfnpExp (id,e1,e2) ->
    IfnpExp(id, exp_subst subst e1, exp_subst subst e2)
  | LetAllocExp (id,e1,simpleTy,e2) ->
    LetAllocExp (id,exp_subst subst e1,simpleTy,exp_subst subst e2)
  | Assign (id1,e1,e2) ->
    Assign(id1, exp_subst subst e1, exp_subst subst e2)
  | AssignInt (id,e1,e2) ->
    AssignInt(id, exp_subst subst e1, exp_subst subst e2)
  | AssignPtr (id1,id2,e) ->
    AssignPtr(id1, id2, exp_subst subst e)
  | Alias (e1,e2,e3) ->
    Alias(exp_subst subst e1, exp_subst subst e2, exp_subst subst e3) 
  | AliasDeref(id1,id2,e) ->
    AliasDeref(id1, id2, exp_subst subst e)
  | AliasAddPtr (id1,id2,i,e) ->
    AliasAddPtr(id1, id2, i, exp_subst subst e)
  | Assert (e1,e2) ->
    Assert(exp_subst subst e1, exp_subst subst e2)
  | Seq (e1,e2) ->
    Seq(exp_subst subst e1, exp_subst subst e2)
  | AppExp (id,es) ->
    AppExp(id, List.map (exp_subst subst) es)
  | EqExp (e1,e2) ->
    EqExp(exp_subst subst e1, exp_subst subst e2)
  | LtExp (e1, e2) ->
    LtExp(exp_subst subst e1, exp_subst subst e2)
  | GtExp (e1, e2) ->
    GtExp(exp_subst subst e1, exp_subst subst e2)
  | LeqExp (e1, e2) ->
    LeqExp(exp_subst subst e1, exp_subst subst e2)
  | GeqExp (e1, e2) ->
    GeqExp(exp_subst subst e1, exp_subst subst e2)
  | NeqExp (e1, e2) ->
    NeqExp(exp_subst subst e1, exp_subst subst e2)
  | AndExp (e1,e2) ->
    AndExp(exp_subst subst e1, exp_subst subst e2)
  | OrExp (e1,e2) ->
    OrExp(exp_subst subst e1, exp_subst subst e2)
  | NotExp e ->
    NotExp (exp_subst subst e)
  | PlusExp (e1,e2) -> 
    PlusExp(exp_subst subst e1, exp_subst subst e2)
  | MinusExp (e1,e2) -> 
    MinusExp(exp_subst subst e1, exp_subst subst e2)
  | MultExp (e1,e2) -> 
    MultExp(exp_subst subst e1, exp_subst subst e2)
  (* | EDiv (e1,e2) -> 
    EDiv(exp_subst subst e1, exp_subst subst e2) *)
  | Var x -> 
    (try 
        lookup x subst
      with
        Error _ -> exp)
  | _ -> exp
  