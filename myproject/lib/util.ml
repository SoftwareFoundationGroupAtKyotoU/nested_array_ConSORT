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
    (print_string ("LetDerefExp( \""^ id1 ^ "\", \"");
      print_string id2;
      print_string "\", ";
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
    (print_string ("AliasDeref( \"" ^ id1 ^ "\", \"");
      print_string id2;
      print_string "\", ";
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
    print_string (sprintf "ILit %s" (Z.to_string i))
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
  | ConstRandInt ->
    print_string "_"
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
    if Z.geq i Z.zero then Id (sprintf "%s" (Z.to_string i)) else Id(sprintf "(-%s)" (Z.to_string i))
  | Var x -> FV x
  | _ -> raise ElimError

  (* 
  i_n -> i_fun_num_varname_b_or_e_nth *)
let rec subst_idx_name exp template =
  match exp with
  | ILit _ -> exp
  | Var v -> 
    let prefix = "i_" in
    let prefix_len = String.length prefix in
    if String.length v < prefix_len then exp
    else if String.sub v 0 prefix_len <> prefix then exp
    else
      let number_str = String.sub v prefix_len (String.length v - prefix_len) in
      let is_digit c = '0' <= c && c <= '9' in
      if number_str = "" then exp
      else if String.for_all is_digit number_str then
        Var (template (int_of_string number_str))
      else exp
  | PlusExp (e1, e2) ->
    let e1' = subst_idx_name e1 template in
    let e2' = subst_idx_name e2 template in
    PlusExp (e1', e2')
  | MinusExp (e1, e2) ->
    let e1' = subst_idx_name e1 template in
    let e2' = subst_idx_name e2 template in
    MinusExp (e1', e2')
  | MultExp (e1, e2) ->
    let e1' = subst_idx_name e1 template in
    let e2' = subst_idx_name e2 template in
    MultExp (e1', e2')
  | _ -> raise (Error "subst_idx_name error")

(* 一次式から変数とその係数の組のリストを抽出する関数 *)
let coeffs exp =
  let rec expand exp =
    match exp with
    | ILit i -> [("", i)]
    | Var v -> [(v, Z.one)]
    | PlusExp (e1, e2) ->
      let lst1 = expand e1 in
      let lst2 = expand e2 in
      lst1 @ lst2
    | MinusExp (e1,e2) -> 
      let lst1 = expand e1 in
      let lst2 = expand e2 in
      lst1 @ 
      List.map (fun (v, c) -> (v, Z.neg c)) lst2
    | MultExp (e1, e2) ->
      (* 乗算の場合、片方が定数でなければ線形式ではないとする *)
      (match e1, e2 with
        | ILit i, e' | e', ILit i -> 
          List.map
          (fun (v', i') -> (v', Z.mul i i'))
          (expand e')
        | _ -> raise (Error "coeffs error"))
    | _ -> raise (Error "coeffs error") in
  let rec simplify lis1 lis2 =
    match lis1 with
    | [] -> lis2
    | (v, c) :: t -> 
      try
        let c' = lookup v lis2 in
        c' := Z.add !c' c;
        simplify t lis2
      with 
      | _ -> simplify t ((v, ref c) :: lis2) in
  List.map
  (fun (v,c) -> 
    (v, Id (sprintf "%s" (Z.to_string !c))))
  (simplify (expand @@ exp) [])

(* リストls1とls2を重複を除いて結合する *)
let rec union_list ls1 ls2 = 
  match ls1 with
  | [] -> ls2
  | x :: ls1' -> if List.mem x ls2 then union_list ls1' ls2 else union_list ls1' (x :: ls2)

(* リストの要素を除去 *)
  let remove_element lst_ref elem =
    let rec remove_first lst =
      match lst with
      | [] -> []
      | x :: xs ->
        if x = elem then xs
        else x :: (remove_first xs)
    in
    lst_ref := remove_first !lst_ref
  
    (* リストの要素の所属判定 *)
  let contains_element lst_ref elem =
    let rec exists lst =
      match lst with
      | [] -> false
      | x :: xs ->
        if x = elem then true
        else exists xs
    in
    exists !lst_ref

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



let ref_depth simpleTy =
  let rec iterative_simplety_depth simpleTy depth = 
    match simpleTy with
    | SRef simpleTy' -> iterative_simplety_depth simpleTy' (depth+1)
    | SInt -> depth
    | _ -> raise (Error "not reference")
  in
  iterative_simplety_depth simpleTy 0

let ftref_depth ftype =
  let rec iterative_ftype_depth ftype depth = 
    match ftype with
    | FTRef (ftype', _, _, _) -> iterative_ftype_depth ftype' (depth+1)
    | FTInt _ -> depth
  in
  iterative_ftype_depth ftype 0
   
let rec depth_to_simpleTy depth =
  if depth <= 0 then
    SInt
  else 
    SRef (depth_to_simpleTy (depth-1))

let deref_simpleTy simpleTy =
  match simpleTy with
  | SRef simpleTy' -> simpleTy'
  | _ -> raise (Error "deref_simpleTy error")

let find_idx_vars var_locations fun_num =
  let rec find_idx_vars_sub id pos branch_trace depth =
    if depth < 1 then []
    else
      let idx = Format.asprintf "i_%d_%s_%d_%dth%a" fun_num id pos depth pp_branch_trace branch_trace in
      idx :: find_idx_vars_sub id pos branch_trace (depth-1)
  in
  List.flatten
    ((List.map
      (fun (id,(pos,branch_trace, simpleTy)) ->
        let depth = ref_depth simpleTy in 
        find_idx_vars_sub id pos branch_trace depth) var_locations ))

let find_idx_vars_be varown_count fun_num =
  let rec find_idx_vars_be_sub id b_or_e depth =
    if depth < 1 then []
    else
      let idx = Format.asprintf "i_%d_%s_%s_%dth" fun_num id b_or_e depth in
      idx :: find_idx_vars_be_sub id b_or_e (depth-1)
  in
  List.flatten
    ((List.map
      (fun (id,b_or_e,fun_num',depth) ->
        if fun_num' = fun_num then
          find_idx_vars_be_sub id b_or_e depth
        else
         []) varown_count ))

(* 最後に評価される式を条件節で場合わけしてリスト *)
let rec ret_of_exp cond exp = 
  match exp with
  | LetIntExp (_,_,e) | LetDerefExp (_,_,e) | LetAllocExp (_,_,_,e) | Assign (_,_,e) 
  | AssignInt (_,_,e) | LetAddPtrExp (_,_,_,e) | AssignPtr (_,_,e) | AliasAddPtr (_,_,_,e)
  | AliasDeref (_,_,e) | Assert (_,e) | Seq (_,e) -> 
    ret_of_exp cond e
  | IfExp (e1,e2,e3) ->
    (* 条件節を場合わけ *)
    let cond_sl = exp_to_smtlib e1 in
    ret_of_exp (cond_sl :: cond) e2 @ ret_of_exp (Not(cond_sl) :: cond) e3
  | _ -> [(cond, exp)]

(* expから自由変数を抜き出す関数 *)
let rec fvs_of_exp exp = 
  match exp with
  | EqExp (e1,e2) | LtExp (e1, e2) | GtExp (e1, e2) | LeqExp (e1, e2) 
  | GeqExp (e1, e2) | AndExp (e1,e2) | OrExp (e1,e2) | PlusExp (e1,e2) 
  | MinusExp (e1,e2) | MultExp (e1,e2) | NeqExp (e1,e2) ->
    let fvs1 = fvs_of_exp e1 in
    let fvs2 = fvs_of_exp e2 in
    fvs1 @ fvs2
  | NotExp e ->
    let fvs = fvs_of_exp e in
    fvs
  | Var x -> [x]
  | _ -> []