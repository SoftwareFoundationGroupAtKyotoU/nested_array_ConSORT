open OUnit2
open Myproject.Syntax
open Myproject.Typing

let test_int _ =
  assert_equal SInt ( ty_test (ILit 1))

let test_ifnp _ =
  assert_equal SInt ( ty_test ( LetBindExp( "x", ILit 1 , IfnpExp( "x", ILit 1 , Var "x" ) ) ) )

let test_alloc _ = 
  assert_equal (SRef SInt) (ty_test (LetBinOpExp( "x", BinOp( Plus , ILit 3 , ILit 5 ) , LetAllocExp( "y", Var "x" , SRef ( SInt ) , Var "y" ) ) ))

let test_assign _ = 
  assert_equal SInt (ty_test ( LetBindExp( "z", ILit 3 , LetAllocExp( "x", Var "z" , SRef ( SInt ) , PreSEMIExpr ( Assign ( "x", "z" ) , LetDerefExp ( "y", Var "x" , Var "y" ) ) ) ) ))

let test_alias _ =
  assert_equal SInt (ty_test (LetBindExp( "z", ILit 3 , LetBindExp( "w", ILit 1 , LetAllocExp( "x", Var "z" , SRef ( SInt ) , LetBinOpExp( "y", BinOp( Plus , Var "x" , Var "w" ) , PreSEMIExpr ( AliasAddPtr ("x", "y", "w" ) , PreSEMIExpr ( AliasDeref ("x", "y" ), ILit 1 ) ) ) ) ) )))

let suite =
  "Typing Test" >::: [
    "test_int" >:: test_int;
    "test_ifnp" >:: test_ifnp;
    "test_alloc" >:: test_alloc;
    "test_assign" >:: test_assign;
    "test_alias" >:: test_alias;
  ]

let () =
  run_test_tt_main suite