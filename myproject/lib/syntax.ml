type id = string
type binOp = Plus | Minus | Mult | Lt | AND | OR | Eq

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
  | PreSEMIExpr of exp * exp
  | Assign of id * id
  | AliasAddPtr of id * id * id
  | AliasDeref of id * id
  | Assert of exp
  | Deref of exp


