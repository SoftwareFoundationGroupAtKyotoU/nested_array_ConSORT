open Syntax
open SmtlibSyntax

exception Error of string

type infer_simple_ty_res = (simpleTy * simpleTy) list * simpleTy [@@deriving show]

let err s = raise (Error s)
let lookup x env =
  try List.assoc x env with Not_found -> err ("variable not bound: " ^ x)

type all_tyenv_type = (id * (id * simpleTy) list) list ref [@@deriving show]
(* (関数名 * (変数 * 単純型)list)list *)
let all_tyenv = ref []
let err s = raise (Error s)

(* 型の単一化のための関数 *)
let rec unify lis =
  match lis with
  [] -> [] (*空集合であれば空の代入を返す*)
  | (tau1, tau2)::rest when tau1 = tau2 -> unify rest 
  | _ -> err("occur error")

(* 型環境 tyenv と式 exp を受け取って，型制約と exp の単純型のペアを返す  *)
let rec infer_simple_ty tyenv exp =
  match exp with
    Var x -> ([], lookup x !tyenv)
  | ILit _ | ConstRandInt _ -> ([], SInt)
  | BLit _ -> ([], SBool)
  | OrExp (exp1, exp2) | AndExp(exp1, exp2) -> 
    let (c1, ty1) = infer_simple_ty tyenv exp1 in
    let (c2, ty2) = infer_simple_ty tyenv exp2 in
    let c3 = unify ((ty1, SBool) :: (ty2, SBool) :: c1 @ c2) in (c3, SBool)
  | NotExp exp ->
    let (c1, ty) = infer_simple_ty tyenv exp in
    let c2 = unify ((ty, SBool) :: c1) in (c2, SBool)
  | EqExp (exp1, exp2) | LtExp (exp1, exp2) | GtExp (exp1, exp2)
  | LeqExp (exp1, exp2) | GeqExp (exp1, exp2) | NeqExp (exp1, exp2) ->
    let (c1, ty1) = infer_simple_ty tyenv exp1 in
    let (c2, ty2) = infer_simple_ty tyenv exp2 in
    let c3 = unify ((ty1, SInt) :: (ty2, SInt) :: c1 @ c2) in (c3, SBool)
  | PlusExp (exp1, exp2) | MinusExp (exp1, exp2) ->
    let (c1, ty1) = infer_simple_ty tyenv exp1 in
    let (c2, ty2) = infer_simple_ty tyenv exp2 in
    let c3 = unify ((ty2, SInt) :: c1 @ c2) in (c3, ty1)
  | MultExp (exp1, exp2) ->
    let (c1, ty1) = infer_simple_ty tyenv exp1 in
    let (c2, ty2) = infer_simple_ty tyenv exp2 in
    let c3 = unify ((ty1, SInt) :: (ty2, SInt) :: c1 @ c2) in (c3, SInt)
  | IfnpExp (id, exp1, exp2) ->
    let tycond = lookup id !tyenv in
    let (c1, ty1) = infer_simple_ty tyenv exp1 in
    let (c2, ty2) = infer_simple_ty tyenv exp2 in
    let c3 = unify ((tycond, SInt) :: (ty1, ty2) :: c1 @ c2) in (c3, ty1)
  | IfExp (exp1, exp2, exp3) ->
    let (c1, ty1) = infer_simple_ty tyenv exp1 in
    let (c2, ty2) = infer_simple_ty tyenv exp2 in
    let (c3, ty3) = infer_simple_ty tyenv exp3 in
    let c4 = unify ((ty1, SBool) :: (ty2, ty3) :: c1 @ c2 @ c3) in (c4, ty2)
  | LetAllocExp (id, exp1, ftype, exp2) ->
    let (c1, ty1) = infer_simple_ty tyenv exp1 in
    tyenv := (id,(ftype_to_simplety ftype)) :: !tyenv;
    let (c2, ty2) = infer_simple_ty tyenv exp2 in
    let c3 = unify ((ty1, SInt) :: c1 @ c2) in (c3, ty2)
  | Let(id, exp1, exp2) ->
    let (c1, ty1) = infer_simple_ty tyenv exp1 in
    tyenv := (id,ty1) :: !tyenv;
    (match exp1, ty1 with
      | Deref id2, SRef sty
        -> tyenv := (id2^"_eq0", SRef (SRef sty)) :: !tyenv; tyenv := (id2^"_non0", SRef (SRef sty)) :: !tyenv;
      | _ -> ());
    let (c2, ty2) = infer_simple_ty tyenv exp2 in
    let c3 = unify ( c1 @ c2) in (c3, ty2)
  | LetImmutAddPtrExp(id1, id2, exp1, exp2) ->
    let (c1, ty1) = infer_simple_ty tyenv exp1 in
    let ty2 = lookup id2 !tyenv in
    assert(ty2 <> SInt);
    tyenv := (id1,ty2) :: !tyenv;
    let (c2, ty3) = infer_simple_ty tyenv exp2 in
    let c3 = unify ( (ty1, SInt)::c1 @ c2) in (c3, ty3)
  | Assign(id, exp1, exp2) ->
    let t = lookup id !tyenv in
    let (c1, ty1) = infer_simple_ty tyenv exp1 in
    let (c2, ty2) = infer_simple_ty tyenv exp2 in
    (match ty1 with
      | SRef _
        -> tyenv := (id^"_eq0", SRef ty1) :: !tyenv; tyenv := (id^"_non0", SRef ty1) :: !tyenv;
      | _ -> ());
    let c3 = unify ((t, SRef (ty1)) :: c1 @ c2) in (c3, ty2)
  | Alias(exp1, exp2, exp3) ->
    let (c1,ty1) = infer_simple_ty tyenv exp1 in
    let (c2,ty2) = infer_simple_ty tyenv exp2 in
    let (c3,ty3) = infer_simple_ty tyenv exp3 in
    let c4 = unify((ty1, ty2) :: c1 @ c2 @ c3) in (c4, ty3)
  | Seq (exp1, exp2) ->
    let (c1,ty1) = infer_simple_ty tyenv exp1 in
    let (c2,ty2) = infer_simple_ty tyenv exp2 in
    let c3 = unify((ty1, SUnit) :: c1 @ c2) in (c3, ty2)
  | Assert(_, exp2) ->
    (* let (c1,ty1) = infer_simple_ty tyenv exp1 in *)
    let (c2,ty2) = infer_simple_ty tyenv exp2 in
    (* let c3 = unify((ty1, SBool) :: c1 @ c2) in (c3, ty2) *)
    (c2, ty2)
  | Assume(exp1, exp2) ->
    let (c1,ty1) = infer_simple_ty tyenv exp1 in
    let (c2,ty2) = infer_simple_ty tyenv exp2 in
    let c3 = unify((ty1, SBool) :: c1 @ c2) in (c3, ty2)
  | Deref id ->
    let ty = lookup id !tyenv in
    (match ty with
    | SRef ty' -> ([], ty')
    | _ -> err ("TyError: Dereferencing integer variables."))
  | AppExp(id, exps) ->
    (match lookup id !tyenv with
    | SFun (simple_arg_types, simple_return_type) ->
      let arg_tys_cs = List.map (infer_simple_ty tyenv) exps in
      let tys = List.map snd arg_tys_cs in
      let cs_arg = List.concat (List.map fst arg_tys_cs) in
      let cs_param = List.map2 (fun x y -> (x,y)) simple_arg_types tys in
      let c_unify = unify( cs_arg @ cs_param) in (c_unify, simple_return_type)
    | _ -> err ("Although " ^ id ^ " is used like a function, " ^ id ^ " isn't function."))
  | Unit -> ([], SUnit)
  | ENull -> err ("TyError: Mismatch Simple Type")
  | DerefBracketExp (id, ids) ->
    let t = lookup id !tyenv in
    let rec deref t ids =
      match ids with
      | [] -> t
      | _ :: tl -> deref (deref_simpleTy t) tl in
    ([], deref t ids)
  | _ -> err("TyError: If this error occurs, the parser is wrong.")

(* 関数のアノテーションから引数と返り値の単純型を求める *)
let rec from_annnotation_to_simpleTy annotation =
  let (args_before_eval, _, return_type) = annotation in
  let types_before_eval = List.map snd args_before_eval in 
  let simple_types_before_eval = List.map convert_to_simpleTy types_before_eval in
  let simple_return_type = convert_to_simpleTy return_type in
  SFun (simple_types_before_eval, simple_return_type)
and convert_to_simpleTy ty =
  match ty with
  | FTInt _ -> SInt
  | FTRef (ft', _, _, _) -> SRef(convert_to_simpleTy ft')

let infer_fdef fun_tyenv fdef = 
  let (fun_name, args, annotation, fun_body) = fdef in
  match from_annnotation_to_simpleTy annotation with
  | SFun (simple_arg_types, simple_return_type) ->
    (* 関数の型を型環境に追加 *)
    let fun_tyenv' = (fun_name, from_annnotation_to_simpleTy annotation) :: fun_tyenv in 
    (* 関数の引数を型環境に追加 *)
    let args_tyenv = List.append (List.map2 (fun x y -> (x, y)) args simple_arg_types) fun_tyenv' in 
    let tyenv = ref args_tyenv in
    let (_, ty) = infer_simple_ty tyenv fun_body in 
    assert(ty = simple_return_type);
    all_tyenv := (fun_name, !tyenv) :: !all_tyenv;
    fun_tyenv'
  | _ -> err ("The function from_annnotation_to_simpleTy must return SFun")

(* プログラム全体を解析して型推論を行い、各関数や式の型を推論する役割を果たす *)
let infer_prog_simpleTy program = 
  let (fdefs, exp) = program in
  let fun_tyenv = List.fold_left infer_fdef [] fdefs in
  let tyenv = ref fun_tyenv in
  let _ = infer_simple_ty tyenv exp in
  (* assert(ty = SUnit); *)
  all_tyenv := ("main", !tyenv) :: !all_tyenv

let subst_arg_name program =
    let find_name subst id = 
      try lookup id subst with | Error _ -> id in
    let rec subst_id_smtlib subst sl =
      let find_name id =
        if starts_with "i" id then id else 
          try (lookup id subst) 
        with | Error _ -> id in
      match sl with
      | FV id -> if starts_with "i" id then sl else FV (find_name id)
      | Id id -> if starts_with "i" id then sl else Id (find_name id)
      | IntPred (id, ids) -> IntPred (find_name id, List.map find_name ids)
      | IntVarPred (n, id, ids) -> IntVarPred (n, find_name id, List.map find_name ids)
      | PtrPred(id, ifel, sls, ids, var_name) -> 
          PtrPred(find_name id, ifel, List.map (subst_id_smtlib subst) sls, List.map find_name ids, var_name)
      | PtrVarPred(n, id, ifel, sls, ids) ->
          PtrVarPred(n, find_name id, ifel, List.map (subst_id_smtlib subst) sls, List.map find_name ids)
      | VarPred | True -> sl
      | _ -> map_smtlib (subst_id_smtlib subst) sl in
    let rec subst_ftype subst ftype =
      match ftype with
      | FTInt sl -> FTInt (subst_id_smtlib subst sl)
      | FTRef (innerty, exp1, exp2, f) 
      -> FTRef(subst_ftype subst innerty, subst_id subst exp1, subst_id subst exp2, f) 
    and subst_id subst exp =
    match exp with
    | Var x -> Var (find_name subst x)
    | ILit _ | BLit _ | Unit | ENull -> exp
    | OrExp _  | AndExp _ | NotExp _ | PlusExp _ | MinusExp _ | ConstRandInt _
    | EqExp _ | LtExp _ | GtExp _ | MultExp _ | IfExp _ | Assume _
    | LeqExp _ | GeqExp _ | NeqExp _ | Alias _ | Seq _ | Assert _-> 
      map_exp (subst_id subst) exp
    | IfnpExp (id, exp1, exp2) ->
      IfnpExp (find_name subst id, subst_id subst exp1, subst_id subst exp2)
    | LetAllocExp (id, exp1, ftype, exp2) ->
      LetAllocExp (find_name subst id, subst_id subst exp1, (subst_ftype subst ftype), subst_id subst exp2)
    | Let(id, exp1, exp2) ->
      Let(find_name subst id, subst_id subst exp1, subst_id subst exp2) 
    | Assign(id, exp1, exp2) ->
      Assign(find_name subst id, subst_id subst exp1, subst_id subst exp2)
    | Deref id ->
      Deref (find_name subst id)
    | AppExp(id, exps) ->
      AppExp(find_name subst id, List.map (subst_id subst) exps)
    | DerefBracketExp (id, exps) ->
      DerefBracketExp (find_name subst id, List.map (subst_id subst) exps)
    | _ -> err("subst_arg_name Error: If this error occurs, the parser is wrong.") in
  let subst_arg_name_sub fdef = 
    let (fun_name, args, annotation, fun_body) = fdef in
    let subst = List.map (fun arg -> (arg, (fun_name ^ arg))) args in
    let new_args = List.map (fun arg -> (fun_name ^ arg)) args in
    let (args_before_eval, args_after_eval, return_type) = annotation in
    let subst_arg arg = 
      match arg with
      | RawId id -> RawId (lookup id subst)
      | HashId id -> HashId (lookup id subst) in
    let new_annotation = 
      (List.map 
      (fun (arg, ftype) -> (subst_arg arg, subst_ftype subst ftype)) 
      args_before_eval, 
      List.map 
      (fun (arg, ftype) -> (subst_arg arg, subst_ftype subst ftype)) 
      args_after_eval, return_type) in
    let new_fun_body = subst_id subst fun_body in
    (fun_name, new_args, new_annotation, new_fun_body) in
  let (fdefs, exp) = program in
  (List.map subst_arg_name_sub fdefs, exp)