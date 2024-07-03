type id = string

type binOp = Plus | Minus | Mult | LT | AND | OR
type tyvar = int

type ty = 
    TyInt
  | TyBool
  | TyVar of tyvar
  | TyFun of ty * tyvar
  | TyRef of ty
  | TyUnit

type simpleTy =
    SInt
  | SRef of simpleTy

type funcallexp = 
    FunCall of id * (id list)

type exp

type presemi =
    Assign of id * id
  | AliasAddPtr of id * id * id
  | AliasDeref of id * id
  | Phi of exp(*ごまかし*)

type exp =
    Var of id
  | ILit of int
  | BLit of bool
  | BinOp of binOp * exp * exp
  | IfnpExp of id * exp * exp
  | LetAllocExp of id * exp * simpleTy * exp
  | LetDerefExp of id * exp * exp
  | LetBinOpExp of id * exp * exp
  | LetBindExp of id * exp * exp
  | LetFunCall of id * funcallexp * exp
  | PreSEMIExpr of presemi * exp

(* 呼び出すたびに，他とかぶらない新しい tyvar 型の値を返す関数 *)
let fresh_tyvar =
  let counter = ref 0 in (* 次に返すべき tyvar 型の値を参照で持っておいて， *)
  let body () =
    let v = !counter in
      counter := v + 1; v (* 呼び出されたら参照をインクリメントして，古い counter の参照先の値を返す *)
  in body


