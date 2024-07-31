open Syntax

exception Error of string

let err s = raise (Error s)

(* Type Environment *)
(* type tyenv = ty Environment.t 型環境を表す型 *)
(* New! 型環境は型スキームへの束縛に *)
type tyenv = ty Environment.t
type subst = (tyvar * ty) list (*型代入を表す型*)

(* 演算子 op が生成すべき制約集合と返り値の型を記述 *)
let ty_prim op ty1 ty2 = match op with
  | Plus -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  | Minus -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  | Mult -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  | Lt -> ([(ty1, TyInt); (ty2, TyInt)], TyBool)
  | AND -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)
  | OR -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)

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
  (* | BinOp (op, exp1, exp2) ->
      let (c1, ty1) = ty_exp tyenv exp1 in
      let (c2, ty2) = ty_exp tyenv exp2 in
      let (c3, ty3) = ty_prim op ty1 ty2 in
	  (* c1 と c2 と　c3 と合わせる *)
      let eqs = c1 @ c2 @ c3 in
	  (* 全体の制約を解く．*)
      (try let c4 = unify eqs in (c4, ty3)
      with Error "occur error3"-> err("occur error"))
      型に異常がある場合は指摘 *)
  | IfnpExp (id, exp1, exp2) ->
      (try 
        (let ty = Environment.lookup x tyenv in
        let (c1, ty1) = ty_exp tyenv exp1 in
        let (c2, ty2) = ty_exp tyenv exp2 in
        let eqs = (ty, SInt) :: c1 @ c2 in
        (try let c3 = unify eqs in (c3, ty1)
        with Error _ -> err("occur error")))
      with Environment.Not_bound -> err ("variable not bound: " ^ x))
  | LetAllocExp (id, exp1, simpleTy, exp2) ->
      let (c1, ty1) = ty_exp tyenv exp1 in
      let extended_tyenv = extend id simpleTy tyenv in
      let (c2, ty2) = ty_exp extended_tyenv exp2 in
      let eqs = (ty1, SInt) :: c1 @ c2 in
      (try let c3 = unify eqs in (c3, ty2)
      with Error _-> err("occur error"))
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
  | AnnoFunExp (id,ty,  exp) ->
      let domty = ty in
    (* id : domty で tyenv を拡張し，その下で exp を型推論 *)
      let s, ranty =
        ty_exp (Environment.extend id (tysc_of_ty domty) tyenv) exp in
        let eqs = (eqs_of_subst s) in
        let s1 = unify eqs in 
        (* let TyScheme (_ , ty) = domty in *)
        (s1, TyFun (subst_type s1 domty, ranty))
  | AppExp (exp1, exp2) -> 
    (*引数と結果の型domty1とranty2を用意*)
      let domty1 = TyVar (fresh_tyvar ()) in
      let ranty2 = TyVar (fresh_tyvar ()) in
      let (s1, ty1) = ty_exp tyenv exp1 in
      let (s2, ty2) = ty_exp tyenv exp2 in
      (* s1とs2を等式制約の集合に変換して，合わせる
         ty1はdomty1->ranty2,ty2はdomty1という制約を付け加える *)
      (* let eqs = [(ty1,TyFun (domty1, ranty2));(ty2, domty1)] @ (eqs_of_subst s1) @ (eqs_of_subst s2) in *)
      (try 
        let eqs = [(ty1,TyFun (domty1, ranty2))] @ (eqs_of_subst s1) in
        let s3 = unify eqs in
          (try 
            let eqs2 = [(ty1,TyFun (domty1, ranty2));(ty2, domty1)] @ (eqs_of_subst s1) @ (eqs_of_subst s2) in
            let s4 = unify eqs2 in (s4, subst_type s4 ranty2)
          with Error "occur error3"-> err("Error in ["^ (string_of_exp exp2)^"] of "^(string_of_exp exp)^
          "\n["^(string_of_exp exp1)^"] is function of type "^(string_of_ty (subst_type s3 ty1))^" but ["^(string_of_exp exp2)^"] has type "^(string_of_ty ty2)^"."))
      with Error "occur error3"-> err("Error in ["^ (string_of_exp exp1)^"] of "^(string_of_exp exp)^
      "\n["^(string_of_exp exp1)^"] has type "^(string_of_ty ty1)^". This is not function."))
      (* let s3 = unify eqs in (s3, subst_type s3 ranty2) *)
  | LetRecExp (id1, id2, exp1, exp2) ->
    (*ダミーの型を用意*)
      let domty1 = TyVar (fresh_tyvar ()) in
      let domty2 = TyVar (fresh_tyvar ()) in
      (* id1 : domty1->domty2, id2 : domty1 で tyenv を拡張し，その下で exp1 を型推論 *)
      let s1, ranty1 =
          ty_exp (Environment.extend id2 (tysc_of_ty(domty1 ))
            (Environment.extend id1 (tysc_of_ty (TyFun (domty1, domty2))) tyenv)) exp1 in
      (* id1 : domty1->domty2で tyenv を拡張し，その下で exp2 を型推論 *)
      let eqs1 =  [(ranty1,domty2)] @ (eqs_of_subst s1) in
      let s_ = unify eqs1 in
      let tyfun = subst_type s_ (TyFun (domty1, ranty1)) in
          let tyscheme = closure tyfun tyenv s1 in
          let s2, ranty2 = ty_exp (Environment.extend id1 tyscheme tyenv) exp2 in
          (* s1とs2を等式制約の集合に変換して，合わせる
         ranty1はdomty2という制約を付け加える *)
      let eqs2 =  (eqs_of_subst s_) @ (eqs_of_subst s2) in
    (* 全体の制約をもう一度解く．*)
      let s3 = unify eqs2 in (s3, subst_type s3 ranty2)
  | BlankList -> ([], TyList (TyVar (fresh_tyvar ())) )
  | AddList (ex, lis) -> 
      let (s1, ty1) = ty_exp tyenv ex in
      let (s2, ty2) = ty_exp tyenv lis in
       (* s1とs2を等式制約の集合に変換して，合わせる
         ty2はty1 listという制約を付け加える *)
      let eqs = [(ty2, TyList ty1)] @ (eqs_of_subst s1) @ (eqs_of_subst s2) in
      let s3 = unify eqs in (s3, subst_type s3 ty2)
  | Match (exp1, exp2, id1, id2, exp3) ->
    (*ダミーの型を用意*)
    let domty1 = TyVar (fresh_tyvar ()) in
    let domty2 = TyVar (fresh_tyvar ()) in
    let s1, ranty1 = ty_exp tyenv exp1 in
    let s2, ranty2 = ty_exp tyenv exp2 in
    (* id1 : domty1, id2 : domty1 list で tyenv を拡張し，その下で exp3 を型推論 *)
    let s3, ranty3 =
        ty_exp (Environment.extend id1 (TyScheme([], domty1)) 
            (Environment.extend id2 (TyScheme ([], (TyList domty1))) tyenv)) exp3 in
    (* s1とs2とs3を等式制約の集合に変換して，合わせる
     ranty1はdomty1 list,ranty2とranty3とdomty2は同じという制約を付け加える *)
    let eqs =  [(ranty1,TyList domty1);(ranty2, domty2);(ranty3, domty2)] 
         @ (eqs_of_subst s1) @ (eqs_of_subst s2)  @ (eqs_of_subst s3)in
  (* 全体の制約をもう一度解く．*)
    let s4 = unify eqs in (s4, subst_type s4 ranty3)
  | Annotation(exp, ty)->
    let (s1, ty1) = ty_exp tyenv exp in
    let eqs = [ty1, ty]@(eqs_of_subst s1) in
    let s2 = unify eqs in (s2, subst_type s2 ty)
  | _ -> err ("Not Implemented!")