type id = string

type binOp = Plus | Minus | Mult | LT | AND | Open_rdonly

type tyvar = int

type ty = 
    TyInt
  | TyBool
  | TyVar of tyvar
  | TyFun of ty * tyvar

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
  | Phi of exp

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


