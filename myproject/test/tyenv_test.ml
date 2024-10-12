open OUnit2
open Myproject.Syntax
open Myproject.Typing
(* 型環境のテスト *)
let test_empty _ =
  assert_equal [] (tyenv_test (ILit 1))

let test_tyint _ =
  assert_equal [("m", SInt)] (tyenv_test (LetBindExp( "m", ILit 1 , ILit 1 )))

let test_int_ref _ = 
  assert_equal [("x", SRef SInt);("n", SInt)] (tyenv_test (LetBindExp( "n", ILit 1 , LetAllocExp( "x", Var "n" , SRef ( SInt ) , PreSEMIExpr ( Assign ( "x", "n" ) , PreSEMIExpr ( Assert (BinOp( Eq , Deref ( Var "x" ) , ILit 1 ) ) , ILit 1 ) ) ) )))

(* let test_assign _ = 
  assert_equal SInt (ty_test ( LetBindExp( "z", ILit 3 , LetAllocExp( "x", Var "z" , SRef ( SInt ) , PreSEMIExpr ( Assign ( "x", "z" ) , LetDerefExp ( "y", Var "x" , Var "y" ) ) ) ) ))

let test_alias _ =
  assert_equal SInt (ty_test (LetBindExp( "z", ILit 3 , LetBindExp( "w", ILit 1 , LetAllocExp( "x", Var "z" , SRef ( SInt ) , LetBinOpExp( "y", BinOp( Plus , Var "x" , Var "w" ) , PreSEMIExpr ( AliasAddPtr ("x", "y", "w" ) , PreSEMIExpr ( AliasDeref ("x", "y" ), ILit 1 ) ) ) ) ) ))) *)

let suite =
  "Tyenv Test" >::: [
    "test_empty" >:: test_empty;
    "test_tyint" >:: test_tyint;
    "test_int_ref" >:: test_int_ref;
    (* "test_assign" >:: test_assign;
    "test_alias" >:: test_alias; *)
  ]

let () =
  run_test_tt_main suite