open Syntax

exception Error of string

let err s = raise (Error s)

type exval =
    IntV of int

let eval_main env = function
  Var x ->
     (try Environment.lookup x env with
     Environment.Not_bound -> err ("Variable not bound: " ^ x))
  | ILit i -> IntV i
  |  _ -> IntV 0

let binop_to_string binop =
  match binop with
    Plus -> "+"
  | Minus -> "-"
  | Mult -> "*"
  | Lt -> "<"
  | AND -> "&&"
  | OR -> "||"
  | Eq -> "="

let rec simpleTy_to_string simpleTy =
  match simpleTy with
  | SInt -> "int "
  | SRef ty -> (simpleTy_to_string ty) ^ "ref "
  | _ -> err ("this type decralation doesn't suit ")

let rec print_ast ast = 
  match ast with
    Var x -> print_string ( "(Var " ^ x ^ ") ")
  | ILit i -> print_string ( "(ILit " ^ string_of_int i ^ ") " )
  | LetBindExp (id , exp , exp2) -> 
      print_string ( "(LET " ^ id ^ "= " );
      print_ast(exp); 
      print_string ( " IN " );
      print_ast(exp2); 
      print_string ") ";
  | LetBinOpExp (id , exp , exp2) -> 
      print_string ( "(LET " ^ id ^ "= " );
      print_ast(exp); 
      print_string ( " IN " );
      print_ast(exp2); 
      print_string ") ";
  | BinOp (binOp , exp , exp2) -> 
      print_string "( ";
      print_ast(exp); 
      let binop = " " ^ (binop_to_string binOp) ^ " " in
      print_string binop;
      print_ast(exp2); 
      print_string ") ";
  | IfnpExp (id , exp , exp2) -> 
      print_string ("( IFNP " ^ id ^ " THEN ");
      print_ast(exp); 
      print_string ("ELSE ");
      print_ast(exp2); 
      print_string (") ");
  |  LetAllocExp ( id , exp , simpleTy , exp2) ->
      print_string ("(LET " ^ id ^ "= ALLOC ");
      print_ast(exp); 
      let simple_type = (simpleTy_to_string simpleTy) in
      print_string (": (" ^ simple_type ^ ") IN ");
      print_ast(exp2);
      print_string (") ");
  | LetDerefExp (id , exp , exp2) ->
      print_string ("(LET " ^ id ^ "= *");
      print_ast(exp); 
      print_string ("IN ");
      print_ast(exp2);
      print_string (") ");
  | PreSEMIExpr (presemi , exp) ->
      print_ast presemi;
      print_ast(exp)
  | Assign (id1, id2) -> 
      print_string id1;
      print_string " := ";
      print_string (id2 ^ "; ")
  | AliasAddPtr (id1, id2, id3) -> 
      print_string (id1 ^ " = " ^ id2 ^ " + " ^ id3 ^ "; ")
  | AliasDeref (id1, id2) -> 
      print_string (id1 ^ " = *" ^ id2 ^ "; ")
  | Assert phi -> 
      print_string "assert";
      print_ast phi;
      print_string "; " 
  | Deref var ->
      print_string "*";
      print_ast var
  | _ ->print_string "3"