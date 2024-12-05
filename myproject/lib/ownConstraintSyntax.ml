open Syntax

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
