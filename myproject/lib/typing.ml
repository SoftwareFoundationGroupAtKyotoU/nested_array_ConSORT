open Syntax

exception Error of string

let err s = raise (Error s)

(* Type Environment *)
(* type tyenv = ty Environment.t 型環境を表す型 *)
(* New! 型環境は型スキームへの束縛に *)
type tyenv = simpleTy Environment.t
(* type subst = (tyvar * ty) list 型代入を表す型 *)

(* 演算子 op が生成すべき制約集合と返り値の型を記述 *)
let ty_prim op ty1 ty2 = match op with
  | Plus -> ([(ty1, SInt); (ty2, SInt)], SInt)
  | Minus -> ([(ty1, SInt); (ty2, SInt)], SInt)
  | Mult -> ([(ty1, SInt); (ty2, SInt)], SInt)
  | Lt -> ([(ty1, SInt); (ty2, SInt)], SBool)
  | AND -> ([(ty1, SBool); (ty2, SBool)], SBool)
  | OR -> ([(ty1, SBool); (ty2, SBool)], SBool)
  | Eq -> ([(ty1,ty2)], SBool)

(*型の単一化のための関数*)
let rec unify lis =
  match lis with
  [] -> [] (*空集合であれば空の代入を返す*)
  | (tau1, tau2)::rest when tau1 = tau2 -> unify rest 
  | _ -> err("occur error")

(* New! 型環境 tyenv と式 exp を受け取って，型制約と exp のシンプル型のペアを返す *)
let rec ty_exp (tyenv: tyenv) exp =
  match exp with
    Var x ->
    (try 
      let ty = Environment.lookup x tyenv in
        ([], ty)
    with Environment.Not_bound -> err ("variable not bound: " ^ x))
  | ILit _ -> ([], SInt)
  | BLit _ -> ([], SBool)
  | BinOp (op, exp1, exp2) ->
      let (c1, ty1) = ty_exp tyenv exp1 in
      let (c2, ty2) = ty_exp tyenv exp2 in
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
        (let ty = Environment.lookup id tyenv in
        let (c1, ty1) = ty_exp tyenv exp1 in
        let (c2, ty2) = ty_exp tyenv exp2 in
        let eqs = (ty, SInt) :: (ty1, ty2) :: c1 @ c2 in
        (try let c3 = unify eqs in (c3, ty1)
        with Error _ -> err("occur error")))
      with Environment.Not_bound -> err ("variable not bound: " ^ id))
  | LetAllocExp (id, exp1, simpleTy, exp2) ->
      let (c1, ty1) = ty_exp tyenv exp1 in
      let extended_tyenv = Environment.extend id simpleTy tyenv in
      let (c2, ty2) = ty_exp extended_tyenv exp2 in
      let eqs = (ty1, SInt) :: c1 @ c2 in
      (try let c3 = unify eqs in (c3, ty2)
      with Error _-> err("occur error"))
  | LetBindExp (id, exp1, exp2) ->
    let (c1, ty1) = ty_exp tyenv exp1 in
    let extended_tyenv = Environment.extend id ty1 tyenv in
    let (c2, ty2) = ty_exp extended_tyenv exp2 in
    let eqs = c1 @ c2 in
    (try let c3 = unify eqs in (c3, ty2)
      with Error _-> err("occur error"))
  | LetBinOpExp (id, exp1, exp2) ->
    let (c1, ty1) = ty_exp tyenv exp1 in
    let extended_tyenv = Environment.extend id ty1 tyenv in
    let (c2, ty2) = ty_exp extended_tyenv exp2 in
    let eqs = c1 @ c2 in
    (try let c3 = unify eqs in (c3, ty2)
      with Error _-> err("occur error"))
  | LetDerefExp (id, exp1, exp2) ->
    let (c1, ty1) = ty_exp tyenv exp1 in
    (match ty1 with 
    | SRef ty2 -> 
      let extended_tyenv = Environment.extend id ty2 tyenv in
      let (c2, ty3) = ty_exp extended_tyenv exp2 in
      let eqs = c1 @ c2 in
      (try let c3 = unify eqs in (c3, ty3)
        with Error _-> err("occur error"))
    | _ -> err("error in deref exp"))
  | PreSEMIExpr (exp1, exp2) ->
    let (c1, ty1) = ty_exp tyenv exp1 in
    let (c2, ty2) = ty_exp tyenv exp2 in
    let eqs = (ty1, SUnit) :: c1 @ c2 in
      (try let c3 = unify eqs in (c3, ty2)
      with Error _-> err("occur error"))
  | Assign (id1, id2) ->
    (try 
      let ty1 = Environment.lookup id1 tyenv in
        (try
          let ty2 = Environment.lookup id2 tyenv in
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
  | _ -> err ("Not Implemented!")

let ty_test exp =
  let (_, ty) = ty_exp Environment.empty exp in
  ty

let tyenv_test exp =
  let tyenv = Environment.empty in
  let _ = ty_exp tyenv exp in
  tyenv