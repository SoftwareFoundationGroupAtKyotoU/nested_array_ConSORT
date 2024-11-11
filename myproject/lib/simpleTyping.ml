open Syntax

exception Error of string

(* (関数名 * (変数 * 単純型)list)list *)
(* let all_tyenv = ref [] *)
(* let err s = raise (Error s)

(* 単純型環境 *)
let simple_tyenv = ref []

(* 環境から変数を探す関数 *)
let lookup x env =
  try List.assoc x env with Not_found -> err ("variable not bound: " ^ x)

  (* 型環境の拡張 *)
let extend_tyenv id ty =
  simple_tyenv := (id,ty) :: !simple_tyenv

(* 演算子 op が生成すべき制約集合と返り値の型を記述 *)
(* let ty_prim op ty1 ty2 = match op with
  | Plus -> ([(ty1, SInt); (ty2, SInt)], SInt)
  | Minus -> ([(ty1, SInt); (ty2, SInt)], SInt)
  | Mult -> ([(ty1, SInt); (ty2, SInt)], SInt)
  | Lt -> ([(ty1, SInt); (ty2, SInt)], SBool)
  | AND -> ([(ty1, SBool); (ty2, SBool)], SBool)
  | OR -> ([(ty1, SBool); (ty2, SBool)], SBool)
  | Eq -> ([(ty1,ty2)], SBool) *)

(*型の単一化のための関数*)
let rec unify lis =
  match lis with
  [] -> [] (*空集合であれば空の代入を返す*)
  | (tau1, tau2)::rest when tau1 = tau2 -> unify rest 
  | _ -> err("occur error")

(* New! 型環境 tyenv と式 exp を受け取って，型制約と exp のシンプル型のペアを返す *)
let rec ty_exp exp =
  match exp with
    Var x ->
    (try 
      let ty = lookup x !simple_tyenv in
        ([], ty)
    with Environment.Not_bound -> err ("variable not bound: " ^ x))
  | ILit _ -> ([], SInt)
  | BLit _ -> ([], SBool)
  | BinOp (op, exp1, exp2) ->
      let (c1, ty1) = ty_exp exp1 in
      let (c2, ty2) = ty_exp exp2 in
      (match ty1 with (* 普通の二項演算の場合*)
      | SInt ->
        let (c3, ty3) = ty_prim op ty1 ty2 in
      (* c1 と c2 と　c3 と合わせる *)
        let eqs = c1 @ c2 @ c3 in
      (* 全体の制約を解く．*)
        (try let c4 = unify eqs in (c4, ty3)
        with Error "occur error3"-> err("occur error3"))
      | SRef _ -> (* ポインタ演算の場合*)
        (match op with
        | Plus | Minus -> 
          let eqs = (ty2, SInt) :: c1 @ c2 in
          (try let c3 = unify eqs in (c3, ty1)
          with Error "occur error3"-> err("occur error3"))
        | _ -> err("binary operation error"))
      | _ -> err "BinOp error")
      (* 型に異常がある場合は指摘 *)
  | IfnpExp (id, exp1, exp2) ->
      (try 
        (let ty = lookup id !simple_tyenv in
        let (c1, ty1) = ty_exp exp1 in
        let (c2, ty2) = ty_exp exp2 in
        let eqs = (ty, SInt) :: (ty1, ty2) :: c1 @ c2 in
        (try let c3 = unify eqs in (c3, ty1)
        with Error _ -> err("occur error")))
      with Environment.Not_bound -> err ("variable not bound: " ^ id))
  | LetAllocExp (id, exp1, simpleTy, exp2) ->
      let (c1, ty1) = ty_exp exp1 in
      extend_tyenv id simpleTy;
      let (c2, ty2) = ty_exp exp2 in
      let eqs = (ty1, SInt) :: c1 @ c2 in
      (try let c3 = unify eqs in (c3, ty2)
      with Error _-> err("occur error"))
  | LetBindExp (id, exp1, exp2) ->
    let (c1, ty1) = ty_exp exp1 in
    extend_tyenv id ty1;
    let (c2, ty2) = ty_exp exp2 in
    let eqs = c1 @ c2 in
    (try let c3 = unify eqs in (c3, ty2)
      with Error _-> err("occur error"))
  | LetBinOpExp (id, exp1, exp2) ->
    let (c1, ty1) = ty_exp exp1 in
    extend_tyenv id ty1;
    let (c2, ty2) = ty_exp exp2 in
    let eqs = c1 @ c2 in
    (try let c3 = unify eqs in (c3, ty2)
      with Error _-> err("occur error"))
  | LetDerefExp (id, exp1, exp2) ->
    let (c1, ty1) = ty_exp exp1 in
    (match ty1 with 
    | SRef ty2 -> 
      extend_tyenv id ty2;
      let (c2, ty3) = ty_exp exp2 in
      let eqs = c1 @ c2 in
      (try let c3 = unify eqs in (c3, ty3)
        with Error _-> err("occur error"))
    | _ -> err("error in deref exp"))
  | PreSEMIExpr (exp1, exp2) ->
    let (c1, ty1) = ty_exp exp1 in
    let (c2, ty2) = ty_exp exp2 in
    let eqs = (ty1, SUnit) :: c1 @ c2 in
      (try let c3 = unify eqs in (c3, ty2)
      with Error _-> err("occur error"))
  | Assign (id1, id2) ->
    (try 
      let ty1 = lookup id1 !simple_tyenv in
        (try
          let ty2 = lookup id2 !simple_tyenv in
          (try let c = unify [ty1, SRef ty2] in (c, SUnit)
        with Error _ -> err("occur error") )
        with Environment.Not_bound -> err ("variable not bound: " ^ id2))
    with Environment.Not_bound -> err ("variable not bound: " ^ id1))
  | AliasAddPtr _ -> ([], SUnit)
  | AliasDeref _ -> ([], SUnit)
  | Assert _ -> ([], SUnit)
  (* | FunExp (id, exp) ->
      (* id の型を表す fresh な型変数を生成 *)
      let domty = TyVar (fresh_tyvar ()) in
	  (* id : domty で tyenv を拡張し，その下で exp を型推論 *)
      let s, ranty =
        ty_exp (Environment.extend id (tysc_of_ty domty) tyenv) exp in
        let eqs = (eqs_of_subst s) in
        let s1 = unify eqs in 
        (* let TyScheme (_ , ty) = domty in *)
        (s1, TyFun (subst_type s1 domty, ranty)) *)
  | _ -> err ("Not Implemented!") *)

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

(* let infer_fdef fun_tyenv fdef = 
  let (fun_name, args, annotation, fun_body) = fdef in
  let SFun (simple_arg_types_eval, simple_return_typ) = from_annnotation_to_simpleTy annotation in 
  (* 関数の型を型環境に追加 *)
  let fun_tyenv' = (fun_name, from_annnotation_to_simpleTy annotation) :: fun_tyenv in 
  (* 関数の引数を型環境に追加 *)
  let args_tyenv = List.append (List.map2 (fun x y -> (x, y)) args simple_arg_types_eval) fun_tyenv' in 
  let tyenv = ref args_tyenv in
  (* let (t, c) = infer_exp tyenv e in 
  let s = ty_unify ((t, t_ret) :: c) in
  let t' = ty_subst s t in
  assert(t' = t_ret);
  let tyenv' = List.map (fun (id,ty) -> (id, ty_subst s ty)) !tyenv in *)
  all_tyenv := (fun_name, tyenv') :: !all_tyenv;
  fun_tyenv'

(* プログラム全体を解析して型推論を行い、各関数や式の型を推論する役割を果たす *)
let infer_prog program = 
  let (fdefs, e) = program in
  let fun_tyenv = List.fold_left infer_fdef [] fdefs in
  let tyenv = ref fun_tyenv in
  let (t, c) = infer_exp tyenv e in
  let s = ty_unify c in
  let t' = ty_subst s t in
  assert(t' = TyUnit);
  let tyenv' = List.map (fun (id,ty) -> (id, ty_subst s ty)) !tyenv in
  all_tyenv := ("main", tyenv') :: !all_tyenv *)

(* let ty_test exp =
  let (_, ty) = ty_exp exp in
  ty

let tyenv_test exp =
  let _ = ty_exp exp in
  !simple_tyenv *)