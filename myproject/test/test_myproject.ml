open OUnit2
open Myproject.Syntax
open Myproject.Typing

let test_fib1 _ =
  assert_equal SInt ( ty_test (ILit 1))

let test_fib2 _ =
  assert_equal SInt ( ty_test (ILit 0) )

(* let test_fib3 _ = 
  assert_equal 1 (Lib.Fib.fib 2)

let test_fib4 _ = 
  assert_equal 2 (Lib.Fib.fib 4) *)

let suite =
  "Fibonacci Test" >::: [
    "test_fib1" >:: test_fib1;
    (* "test_fib2" >:: test_fib2; *)
    (* "test_fib3" >:: test_fib3;
    "test_fib4" >:: test_fib4; *)
  ]

let () =
  run_test_tt_main suite