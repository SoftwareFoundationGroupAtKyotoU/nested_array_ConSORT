open Syntax
(* open Util *)

exception Error of string

let err s = raise (Error s)
let lookup x env =
  try List.assoc x env with Not_found -> err ("variable not bound: " ^ x)

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
  | ILit _ | Nondet -> ([], SInt)
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
  | LetAllocExp (id, exp1, simpleTy, exp2) ->
    let (c1, ty1) = infer_simple_ty tyenv exp1 in
    tyenv := (id,simpleTy) :: !tyenv;
    let (c2, ty2) = infer_simple_ty tyenv exp2 in
    let c3 = unify ((ty1, SInt) :: c1 @ c2) in (c3, ty2)
  | Let(id, exp1, exp2) ->
    let (c1, ty1) = infer_simple_ty tyenv exp1 in
    tyenv := (id,ty1) :: !tyenv;
    let (c2, ty2) = infer_simple_ty tyenv exp2 in
    let c3 = unify ( c1 @ c2) in (c3, ty2)
  | Assign(id, exp1, exp2) ->
    let t = lookup id !tyenv in
    let (c1, ty1) = infer_simple_ty tyenv exp1 in
    let (c2, ty2) = infer_simple_ty tyenv exp2 in
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
  | Assert(exp1, exp2) ->
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
  let (_, ty) = infer_simple_ty tyenv exp in
  (* assert(ty = SUnit); *)
  all_tyenv := ("main", !tyenv) :: !all_tyenv

(* let ty_test exp =
  let (_, ty) = infer_simple_ty exp in
  ty

let tyenv_test exp =
  let _ = infer_simple_ty exp in
  !simple_tyenv *)