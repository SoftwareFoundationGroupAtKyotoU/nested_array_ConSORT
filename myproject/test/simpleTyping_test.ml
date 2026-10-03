(* open OUnit2
open Myproject.Syntax
open Myproject.SimpleTyping



(* 単純型推論のテスト *)
let test_empty _ =
  let expected = ([], SInt) in
  let actual = (infer_simple_ty (ref []) (ILit Z.one)) in
  assert_equal expected actual ~printer:show_infer_simple_ty_res

let test_tyint _ =
  let expected = ([], SInt) in
  let actual = (infer_simple_ty (ref []) (Let( "m", ILit Z.one , ILit Z.one )) ) in
  assert_equal expected actual ~printer:show_infer_simple_ty_res

let test_init_10 _ =
  let expected = ([], SUnit) in
  let actual = (infer_simple_ty (ref [("init", SFun([SInt; SRef SInt], SInt) ); ("verify", SFun([SInt; SRef SInt], SInt) )]) ( LetAllocExp( "p", ILit (Z.of_int 10), SRef SInt, Let( "m", ILit (Z.of_int 10), Let( "d", AppExp( "init", [Var "m"; Var "p"]), Let( "d2", AppExp( "verify", [Var "m"; Var "p"]), Unit)))) )) in
  assert_equal expected actual ~printer:show_infer_simple_ty_res
  
  
let test_int_ref _ = 
  let oc = open_in "/Users/fujiwarayuusuke/nested_array_ConSORT/myproject/example/init_10.imp" in
  let program = Myproject.Parser.toplevel Myproject.Lexer.main (Lexing.from_channel oc) in
  close_in oc;
  infer_prog_simpleTy program;
  let expected = ref ([("main",
  [("d2", SInt); ("d", SInt); ("m", SInt);
    ("p", (SRef SInt));
    ("verify",
     (SFun ([SInt; (SRef SInt)], SInt)));
    ("init",
     (SFun ([SInt; (SRef SInt)], SInt)))
    ]);
  ("verify",
   [("d", SInt); ("m", SInt);
     ("q", (SRef SInt)); ("y", SInt);
     ("n", SInt); ("p", (SRef SInt));
     ("verify",
      (SFun ([SInt; (SRef SInt)],
         SInt)));
     ("init",
      (SFun ([SInt; (SRef SInt)],
         SInt)))
     ]);
  ("init",
   [("d", SInt); ("m", SInt);
     ("q", (SRef SInt)); ("n", SInt);
     ("p", (SRef SInt));
     ("init",
      (SFun ([SInt; (SRef SInt)],
         SInt)))
     ])
  ]) in
  let actual = all_tyenv in
  assert_equal expected actual ~printer:show_all_tyenv_type

(* let test_assign _ = 
  assert_equal SInt (ty_test ( LetBindExp( "z", ILit 3 , LetAllocExp( "x", Var "z" , SRef ( SInt ) , PreSEMIExpr ( Assign ( "x", "z" ) , LetDerefExp ( "y", Var "x" , Var "y" ) ) ) ) ))

let test_alias _ =
  assert_equal SInt (ty_test (LetBindExp( "z", ILit 3 , LetBindExp( "w", ILit 1 , LetAllocExp( "x", Var "z" , SRef ( SInt ) , LetBinOpExp( "y", BinOp( Plus , Var "x" , Var "w" ) , PreSEMIExpr ( AliasAddPtr ("x", "y", "w" ) , PreSEMIExpr ( AliasDeref ("x", "y" ), ILit 1 ) ) ) ) ) ))) *)

let suite =
  "Tyenv Test" >::: [
    "test_empty" >:: test_empty;
    "test_tyint" >:: test_tyint;
    "test_init_10" >:: test_init_10;
    "test_int_ref" >:: test_int_ref;
    (* "test_assign" >:: test_assign;
    "test_alias" >:: test_alias; *)
  ]

let () =
  run_test_tt_main suite *)