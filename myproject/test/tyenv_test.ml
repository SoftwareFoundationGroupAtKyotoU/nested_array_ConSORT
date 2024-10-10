open OUnit2
open Myproject.Syntax
open Myproject.Typing
open Myproject.Environment
(* 型環境のテスト *)
let test_empty _ =
  assert_equal empty (tyenv_test (ILit 1))

let test_tyint _ =
  assert_equal empty (tyenv_test (LetBindExp( "n", ILit 1 , ILit 1 )))

(* let test_alloc _ = 
  assert_equal (SRef SInt) (ty_test (LetBinOpExp( "x", BinOp( Plus , ILit 3 , ILit 5 ) , LetAllocExp( "y", Var "x" , SRef ( SInt ) , Var "y" ) ) ))

let test_assign _ = 
  assert_equal SInt (ty_test ( LetBindExp( "z", ILit 3 , LetAllocExp( "x", Var "z" , SRef ( SInt ) , PreSEMIExpr ( Assign ( "x", "z" ) , LetDerefExp ( "y", Var "x" , Var "y" ) ) ) ) ))

let test_alias _ =
  assert_equal SInt (ty_test (LetBindExp( "z", ILit 3 , LetBindExp( "w", ILit 1 , LetAllocExp( "x", Var "z" , SRef ( SInt ) , LetBinOpExp( "y", BinOp( Plus , Var "x" , Var "w" ) , PreSEMIExpr ( AliasAddPtr ("x", "y", "w" ) , PreSEMIExpr ( AliasDeref ("x", "y" ), ILit 1 ) ) ) ) ) ))) *)

let suite =
  "Tyenv Test" >::: [
    "test_empty" >:: test_empty;
    "test_tyint" >:: test_tyint;
    (* "test_alloc" >:: test_alloc;
    "test_assign" >:: test_assign;
    "test_alias" >:: test_alias; *)
  ]

let () =
  run_test_tt_main suite