exception Error of string

type id = string
(* type binOp = Plus | Minus | Mult | Lt | AND | OR | Eq *)

(** Type representing the syntax of the SMT-LIB language *)
type smtlib = 
  | VarPred

type tyvar = int

(* 単純型 *)
type simpleTy =
    SInt
  | SRef of simpleTy
  | SBool
  | SUnit
  | SFun of simpleTy list * simpleTy
  | SVar of tyvar

let rec print_simplety simpleTy = 
  match simpleTy with
  | SInt -> print_string "int"
  | SRef simpleTy' -> 
    (print_simplety simpleTy';
    print_string " ref")
  | _ -> raise (Error "perhaps source program error")

type funcallexp = 
    FunCall of id * (id list)

type exp =
    Var of id
  | ILit of int
  | BLit of bool
  (* | BinOp of binOp * exp * exp *)
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
  | LetAllocExp of id * exp * simpleTy * exp
  | LetIntExp of id * exp * exp
  | LetAddPtrExp of id * id * exp * exp
  | LetDerefExp of id * id * exp
  (* | LetBinOpExp of id * exp * exp *)
  (* | LetBindExp of id * exp * exp *)
  (* | LetFunCall of id * funcallexp * exp *)
  | Let of id * exp * exp
  (* | PreSEMIExpr of exp * exp *)
  | Assign of id * exp * exp
  | AssignInt of id * exp * exp
  | AssignPtr of id * id * exp
  | AliasAddPtr of id * id * exp * exp
  | AliasDeref of id * id * exp
  | Alias of exp * exp * exp
  | Seq of exp * exp
  | Assert of exp * exp
  | Deref of id
  | AppExp of id * exp list
  | Nondet
  | Unit
  | ENull

(* 篩型と所有権付きの型 *)
type ftype =
  | FTInt of smtlib (** Refinement predicats are described usign the SMT-LIB language *)
  | FTRef of ftype * exp * exp * float  (** Ownership functions are restricted to the form \[l, u\] |-> o, where l : exp, u : exp and o : float *)

type ftype_id =
  | RawId of id
  | HashId of id

type annotation = (ftype_id * ftype) list * (ftype_id * ftype) list * ftype
type fdef = id * id list * annotation * exp
type program = fdef list * exp


