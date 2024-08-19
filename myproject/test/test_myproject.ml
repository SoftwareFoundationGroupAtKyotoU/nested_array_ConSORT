open OUnit2
open Myproject.Syntax
open Myproject.Typing

let test_fib1 _ =
  assert_equal SInt ( ty_test (ILit 1))

let test_fib2 _ =
  assert_equal SInt ( ty_test ( LetBindExp( "x", ILit 1 , IfnpExp( "x", ILit 1 , Var "x" ) ) ) )

let test_fib3 _ = 
  assert_equal (SRef SInt) (ty_test (LetBinOpExp( "x", BinOp( Plus , ILit 3 , ILit 5 ) , LetAllocExp( "y", Var "x" , SRef ( SInt ) , Var "y" ) ) ))

let test_fib4 _ = 
  assert_equal SInt (ty_test ( LetBindExp( "z", ILit 3 , LetAllocExp( "x", Var "z" , SRef ( SInt ) , PreSEMIExpr ( Assign ( "x", "z" ) , LetDerefExp ( "y", Var "x" , Var "y" ) ) ) ) ))

let test5 _ =
  assert_equal SInt (ty_test (LetBindExp( "z", ILit 3 , LetBindExp( "w", ILit 1 , LetAllocExp( "x", Var "z" , SRef ( SInt ) , LetBinOpExp( "y", BinOp( Plus , Var "x" , Var "w" ) , PreSEMIExpr ( AliasAddPtr ("x", "y", "w" ) , PreSEMIExpr ( AliasDeref ("x", "y" ), ILit 1 ) ) ) ) ) )))

let suite =
  "Fibonacci Test" >::: [
    "test_fib1" >:: test_fib1;
    "test_fib2" >:: test_fib2;
    "test_fib3" >:: test_fib3;
    "test_fib4" >:: test_fib4;
    "test5" >:: test5;
  ]

let () =
  run_test_tt_main suite