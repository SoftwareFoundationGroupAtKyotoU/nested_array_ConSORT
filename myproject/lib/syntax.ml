open SmtlibSyntax
exception Error of string

type id = string [@@deriving show]

type tyvar = int [@@deriving show]

(* 単純型 *)
type simpleTy =
    SInt
  | SRef of simpleTy
  | SBool
  | SUnit
  | SFun of simpleTy list * simpleTy
  | SVar of tyvar
  [@@deriving show]

let rec print_simplety simpleTy = 
  match simpleTy with
  | SInt -> print_string "SInt"
  | SRef simpleTy' -> 
    (print_string "SRef ";
    print_simplety simpleTy')
  | _ -> raise (Error "perhaps source program error")

(* 単純型のフォーマッタ *)
let rec pp_simpleTy fmt simpleTy =
  match simpleTy with
  | SInt -> Format.fprintf fmt "int"
  | SRef simpleTy' -> 
    Format.fprintf fmt "%a ref" pp_simpleTy simpleTy'
  | SBool -> Format.fprintf fmt "bool"
  | SUnit -> Format.fprintf fmt "unit"
  | SVar tyvar -> Format.fprintf fmt "'%d" tyvar
  | SFun (ty_list, ty) -> 
    let _ = List.map (fun ty -> Format.fprintf fmt "%a -> " pp_simpleTy ty) ty_list in
    Format.fprintf fmt "%a" pp_simpleTy ty

let deref_simpleTy simpleTy =
  match simpleTy with
  | SRef simpleTy' -> simpleTy'
  | _ -> raise (Error "deref_simpleTy error")

type funcallexp = 
    FunCall of id * (id list)

type exp =
    Var of id
  | ILit of Z.t
  | BLit of bool
  | OrExp of exp * exp
  | AndExp of exp * exp
  | NotExp of exp
  | EqExp of exp * exp
  | LtExp of exp * exp
  | GtExp of exp * exp
  | LeqExp of exp * exp
  | GeqExp of exp * exp
  | NeqExp of exp * exp
  | PlusExp of exp * exp
  | MinusExp of exp * exp
  | MultExp of exp * exp
  | IfnpExp of id * exp * exp
  | IfExp of exp * exp * exp
  | LetAllocExp of id * exp * ftype * exp
  | LetIntExp of id * exp * exp
  | LetAddPtrExp of id * id * exp * exp
  | LetImmutAddPtrExp of id * id * exp * exp
  | LetDerefExp of id * id * exp
  | Let of id * exp * exp
  | Assign of id * exp * exp
  | AssignInt of id * exp * exp
  | AssignPtr of id * id * exp
  | AliasAddPtr of id * id * exp * exp
  | AliasDeref of id * id * exp
  | Alias of exp * exp * exp
  | Seq of exp * exp
  | Assert of exp * exp
  | Assume of exp * exp
  | Deref of id
  | DerefBracketExp of id * exp list (* id [n]...で配列のn番目にアクセス*)
  | AppExp of id * exp list
  | Unit
  | ENull
  | ConstRandInt of exp
and
(* 篩型と所有権付きの型 *)
  ftype =
  | FTInt of smtlib (** Refinement predicats are described usign the SMT-LIB language *)
  | FTRef of ftype * exp * exp * float  (** Ownership functions are restricted to the form \[l, u\] |-> o, where l : exp, u : exp and o : float *)

let rec ftype_to_simplety ftype =
  match ftype with
  | FTInt _ -> SInt
  | FTRef (innerty,_,_,_) -> SRef (ftype_to_simplety innerty)



let rec map_exp f exp =
  match exp with
  | IfnpExp _ | LetAllocExp _ | Let _ | Assign _ | Deref _ | AppExp _ | Var _
  | Unit | ENull | DerefBracketExp _ | ILit _ | BLit _ 
  | LetIntExp _ | LetAddPtrExp _ | LetImmutAddPtrExp _| LetDerefExp _ | AssignInt _ | AssignPtr _ 
  | AliasAddPtr _ | AliasDeref _ -> f exp
  | Seq (exp1, exp2) ->
    Seq (f exp1,f exp2)
  | Assert(exp1, exp2) ->
    Assert (f exp1,f exp2)
  | Assume(exp1, exp2) ->
    Assume (f exp1,f exp2)
  | OrExp (exp1, exp2)  -> 
    OrExp (f exp1,f exp2)
  | AndExp(exp1, exp2) ->
    AndExp(f exp1,f exp2)
  | NotExp exp ->
    NotExp(f exp)
  | EqExp (exp1, exp2) ->
    EqExp(f exp1,f exp2)
  | LtExp (exp1, exp2) ->
    LtExp(f exp1,f exp2)
  | GtExp (exp1, exp2) ->
    GtExp(f exp1,f exp2)
  | LeqExp (exp1, exp2) ->
    LeqExp(f exp1,f exp2)
  | GeqExp (exp1, exp2) ->
    GeqExp(f exp1, f exp2)
  | NeqExp (exp1, exp2) ->
    NeqExp(f exp1,f exp2)
  | PlusExp (exp1, exp2) ->
    PlusExp(f exp1,f exp2)
  | MinusExp (exp1, exp2) ->
    MinusExp(f exp1,f exp2)
  | MultExp (exp1, exp2) ->
    MultExp(f exp1,f exp2)
  | IfExp (exp1, exp2, exp3) ->
    IfExp (f exp1,f exp2,f exp3)
  | Alias(exp1, exp2, exp3) ->
    Alias (f exp1,f exp2,f exp3)
  | ConstRandInt exp ->
    ConstRandInt (f exp)



let ftref_depth ftype = 
  let rec iterative_ftref_depth ftype depth =
    match ftype with
    | FTInt _ -> depth
    | FTRef (ftype', _, _, _) -> iterative_ftref_depth ftype' (depth+1)
  in iterative_ftref_depth ftype 0

type ftype_id =
  | RawId of id
  | HashId of id

type annotation = (ftype_id * ftype) list * (ftype_id * ftype) list * ftype
type fdef = id * id list * annotation * exp
type program = fdef list * exp

(* 所有範囲と所有権の値をまとめたデータ構造 *)
type own_represent = 
  | UnitRange of smtlib * smtlib * smtlib
  | DivRange of (smtlib * smtlib) * (smtlib) * (smtlib * smtlib)
  | NestedUnitRange of smtlib * smtlib * smtlib * own_represent

let starts_with prefix s =
  let prefix_len = String.length prefix in
  String.length s >= prefix_len && String.sub s 0 prefix_len = prefix

let ends_with suffix s =
  let suffix_len = String.length suffix in
  let s_len = String.length s in
  s_len >= suffix_len &&
  String.sub s (s_len - suffix_len) suffix_len = suffix