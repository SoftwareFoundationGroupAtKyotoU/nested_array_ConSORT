(* open Syntax *)

type id = string

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
  (* 以下篩型用 *)
  | IntPred of id * id list
  | IntVarPred of int * id * id list
  | PtrPred of id * id * smtlib * id list
  | PtrVarPred of int * id * id * smtlib * id list
  (* 篩型込みのポインタ，関数番号*変数名*b or e*添え字を表す変数*依存できる変数リスト *)
  | VarPred
  | Ands of smtlib list
  | True