type id = string
type binOp = Plus | Minus | Mult | Lt | AND | OR

type ty = 
    TyInt
  | TyBool
  | TyRef of ty
  | TyUnit

type simpleTy =
    SInt
  | SRef of simpleTy
  | SBool
  | SUnit

type funcallexp = 
    FunCall of id * (id list)

type phi =
    Phi of id

type presemi =
    Assign of id * id
  | AliasAddPtr of id * id * id
  | AliasDeref of id * id
  | Assert of phi
  | Phi(*ごまかし*)

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
  | ExpSeq of presemi * exp


