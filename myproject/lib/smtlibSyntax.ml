open Syntax

(** Type representing the syntax of the SMT-LIB language *)
type smtlib = 
  | Or of smtlib * smtlib
  | And of smtlib * smtlib
  | Imply of smtlib * smtlib
  | Not of smtlib
  | Eq of smtlib * smtlib
  | Lt of smtlib * smtlib
  | Gt of smtlib * smtlib
  | Leq of smtlib * smtlib
  | Geq of smtlib * smtlib
  | Add of smtlib * smtlib
  | Sub of smtlib * smtlib
  | Mul of smtlib * smtlib
  (* | Div of smtlib * smtlib *)
  | FV of id
  | Id of id
  | IntPred of id * id list
  | IntVarPred of int * id * id list
  | PtrPred of id * id * smtlib * id list
  | PtrVarPred of int * id * id * smtlib * id list
  | VarPred
  | Ands of smtlib list