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
    Plus -> "Plus"
  | Minus -> "Minus"
  | Mult -> "Mult"
  | Lt -> "Lt"
  | AND -> "AND"
  | OR -> "OR"
  | Eq -> "Eq"

let rec simpleTy_to_string simpleTy =
  match simpleTy with
  | SInt -> "SInt "
  | SRef ty -> "SRef ( " ^ (simpleTy_to_string ty) ^ ") "
  | _ -> err ("this type decralation doesn't suit ")

let rec print_ast ast = 
  match ast with
    Var x -> print_string ( "Var \"" ^ x ^ "\" ")
  | ILit i -> print_string ( "ILit " ^ string_of_int i ^ " " )
  | LetBindExp (id , exp , exp2) -> 
      print_string ( "LetBindExp( \"" ^ id ^ "\", ");
      print_ast(exp); 
      print_string (", ");
      print_ast(exp2); 
      print_string ") ";
  | LetBinOpExp (id , exp , exp2) -> 
      print_string ( "LetBinOpExp( \"" ^ id ^ "\", " );
      print_ast(exp); 
      print_string ( ", " );
      print_ast(exp2); 
      print_string ") ";
  | BinOp (binOp , exp , exp2) -> 
      print_string "BinOp( "; 
      let binop = (binop_to_string binOp) ^ " " in
      print_string (binop ^ ", ");
      print_ast(exp);
      print_string (", ");
      print_ast(exp2); 
      print_string ") ";
  | IfnpExp (id , exp , exp2) -> 
      print_string ("IfnpExp( \"" ^ id ^ "\", ");
      print_ast(exp); 
      print_string (", ");
      print_ast(exp2); 
      print_string (") ");
  |  LetAllocExp ( id , exp , simpleTy , exp2) ->
      print_string ("LetAllocExp( \"" ^ id ^ "\", ");
      print_ast(exp); 
      print_string ", ";
      let simple_type = (simpleTy_to_string simpleTy) in
      print_string ( simple_type ^ ", ");
      print_ast(exp2);
      print_string (") ");
  | LetDerefExp (id , exp , exp2) ->
      print_string (( "LetDerefExp ( \"") ^ id ^ "\", ");
      print_ast exp;
      print_string ", ";
      print_ast exp2;
      print_string ") "
  | PreSEMIExpr (presemi , exp) ->
      print_string ("PreSEMIExpr ( ");
      print_ast presemi;
      print_string ", ";
      print_ast(exp);
      print_string ") "
  | Assign (id1, id2) -> 
      print_string ("Assign ( \"" ^ id1 ^ "\", \"" ^ id2 ^ "\" ) " )
  | AliasAddPtr (id1, id2, id3) -> 
      print_string ("AliasAddPtr (\"" ^ id1 ^ "\", \"" ^ id2 ^ "\", \"" ^ id3 ^ "\" ) ")
  | AliasDeref (id1, id2) -> 
      print_string ("AliasDeref (\"" ^ id1 ^ "\", \"" ^ id2 ^ "\" )")
  | Assert phi -> 
      print_string "Assert (";
      print_ast phi;
      print_string ") " 
  | Deref var ->
      print_string "Deref ( ";
      print_ast var;
      print_string ") "
  | _ ->print_string "3"