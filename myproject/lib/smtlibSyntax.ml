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
  | IntPred of id * id list (* 変数名，引数リスト *)
  | IntVarPred of int * id * id list (* 関数番号，変数名，引数リスト *)
  | PtrPred of id * id * smtlib list * id list (* 変数名，分岐の文字列，添え字リスト，引数リスト *)
  | PtrVarPred of int * id * id * smtlib list * id list (* 関数番号，変数名，分岐の文字列，添え字リスト，引数リスト *)
  (* 篩型込みのポインタ，関数番号*変数名*b or e*添え字を表す変数*依存できる変数リスト *)
  | VarPred
  | Ands of smtlib list
  | True

let rec map_smtlib f smtlib =
  match smtlib with
  | FV _ | Id _ | IntPred _ | IntVarPred _ | PtrPred _ 
  | PtrVarPred _ | VarPred | True -> smtlib
  | Or(sl1, sl2) -> Or(map_smtlib f sl1, map_smtlib f sl2)
  | And(sl1, sl2) -> And(map_smtlib f sl1, map_smtlib f sl2)
  | Imply(sl1, sl2) -> Imply(map_smtlib f sl1, map_smtlib f sl2)
  | Not sl -> Not (map_smtlib f sl)
  | Eq(sl1, sl2) -> Eq(map_smtlib f sl1, map_smtlib f sl2)
  | Lt(sl1, sl2) -> Lt(map_smtlib f sl1, map_smtlib f sl2)
  | Gt(sl1, sl2) -> Gt(map_smtlib f sl1, map_smtlib f sl2)
  | Leq(sl1, sl2) -> Leq(map_smtlib f sl1, map_smtlib f sl2)
  | Geq(sl1, sl2) -> Geq(map_smtlib f sl1, map_smtlib f sl2)
  | Add(sl1, sl2) -> Add(map_smtlib f sl1, map_smtlib f sl2)
  | Sub(sl1, sl2) -> Sub(map_smtlib f sl1, map_smtlib f sl2)
  | Mul(sl1, sl2) -> Mul(map_smtlib f sl1, map_smtlib f sl2)
  | Ands sls -> Ands (List.map f sls)