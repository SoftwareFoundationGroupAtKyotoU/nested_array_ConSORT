open Myproject.Output
open Myproject.Cexample

let execute_main file_name unsat_core_enabled insert_alias random_assignment=
  let start_time = Unix.gettimeofday () in
  (try 
      let iter = ref 0 in
      let continue = ref true in
      let oc = open_out "experiment/result_cex_all" in
      output_string oc "counter example list\n";
      flush oc;
      let result_path = (Format.sprintf "experiment/own_result/result_%s" (Filename.basename file_name)) in
      let oc = open_out result_path in
      flush oc;
      close_out oc;
      while !continue do
        generate_constrs file_name !iter insert_alias random_assignment;
        let _ = Sys.command "z3 parallel.enable=true smt.threads=4 experiment/out_int.smt2 > experiment/result_int" in
        (* let _ = Sys.command "/usr/bin/time -v z3 experiment/out_int.smt2 > experiment/result_int" in *)
        let ic = open_in "experiment/result_int" in
        let first_line = input_line ic in
        (if first_line = "unknown" then 
          (
            (* Printf.printf "iter: %d unknown " !iter; *)
          (* let end_time = Unix.gettimeofday () in *)
          (* Printf.printf "time: %fs\n" (end_time -. start_time); *)
          flush stdout;
          add_sample ()
          )
        else if first_line = "unsat" then
          (Printf.printf "iter: %d unsat " !iter;
          let end_time = Unix.gettimeofday () in
          Printf.printf "time: %fs\nprogram error \n" (end_time -. start_time);
          flush stdout;
          continue := false;
          exit 0;
          )
        else if first_line = "sat" then
          (
            Printf.printf "int fin \n";
          flush stdout;
          main_fv file_name insert_alias;
          let _ = Sys.command (Format.sprintf "z3 experiment/out_fv.smt2 > %s" result_path) in
          Printf.printf "fv fin ";
          let ic = open_in result_path in
          (* 最初の行を読み取る *)
          let first_line = input_line ic in
          (* ファイルを閉じる *)
          close_in ic;
          (* 比較して結果を出力 *)
          if first_line = "sat" then 
            (let end_time = Unix.gettimeofday () in
            Printf.printf "iter: %d sat own time: %fs\nownership: sat\n" !iter (end_time -. start_time);
            continue := false) 
          else 
            (
              Printf.printf "iter: %d unsat " !iter;
            let end_time = Unix.gettimeofday () in
            Printf.printf "time: %fs\n" (end_time -. start_time);
            main_cexample file_name insert_alias;
            flush stdout;
            let _ = Sys.command "z3 experiment/out_cexample.smt2 > experiment/result_cexample" in
            flush stdout;
            create_cexapmle ();
            Printf.printf "cex fin ";
            ))
          else
            (Printf.printf "implement error\n %s\n" first_line;
            let end_time = Unix.gettimeofday () in
            Printf.printf "time: %fs\nprogram error \n" (end_time -. start_time);
            flush stdout;
            continue := false;
            exit 0;
            ));
        flush stdout;
        iter := !iter + 1      
      done;
      let ref_start = Unix.gettimeofday () in
      (main_sat_ans file_name insert_alias;
      main_chc file_name result_path true insert_alias;
      let _ = Sys.command "hoice experiment/out_chc.smt2 > experiment/chc_result" in
      let ic = open_in "experiment/chc_result" in
      (* 最初の行を読み取る *)
      let first_line = input_line ic in
      Printf.printf "ref time %fs\nrefinement: %s\ntotal time: %fs\n" (Unix.gettimeofday () -. ref_start) first_line (Unix.gettimeofday () -. start_time);
      flush stdout;
      (if first_line = "unsat" && unsat_core_enabled then
        (
          (* main_chc file_name result_path true; *)
        let _ = Sys.command "z3 parallel.enable=true smt.threads=4 experiment/out_chc.smt2 > experiment/chc_result" in
        ()));
      flush stdout;)
    with Sys_error msg -> Printf.eprintf "Error: %s\n" msg
    | End_of_file -> Printf.printf "canceled"
    (* | _ -> 
      Printf.eprintf "Error: %s\n" "unsat";
      let end_time = Unix.gettimeofday () in
      Printf.printf "time: %fs\n" (end_time -. start_time) *)
      )

let execute_main_annotated file_name unsat_core_enabled insert_alias random_assignment=
  let start_time = Unix.gettimeofday () in
  (try 
      let oc = open_out "experiment/result_cex_all" in
      output_string oc "counter example list\n";
      flush oc;
      let result_path = (Format.sprintf "experiment/own_result/result_%s" (Filename.basename file_name)) in
      let oc = open_out result_path in
      close_out oc;
      (* while !continue do *)
      generate_constrs_annotated file_name insert_alias random_assignment;
      main_sat_ans file_name insert_alias;
      let ref_start = Unix.gettimeofday () in
      Printf.printf "own time %fs\n" (ref_start -. start_time) ;
      flush stdout;
      main_chc file_name result_path true insert_alias;
      let _ = Sys.command "hoice experiment/out_chc.smt2 > experiment/chc_result" in
      let ic = open_in "experiment/chc_result" in
      (* 最初の行を読み取る *)
      let first_line = input_line ic in
      Printf.printf "ref time %fs\nrefinement: %s\ntotal time: %fs\n" (Unix.gettimeofday () -. ref_start) first_line (Unix.gettimeofday () -. start_time);
      flush stdout;
      (if first_line = "unsat" && unsat_core_enabled then
        (
          (* main_chc file_name result_path true; *)
        let _ = Sys.command "z3 parallel.enable=true smt.threads=4 experiment/out_chc.smt2 > experiment/chc_result" in
        ()));
      flush stdout;
        (* let _ = Sys.command "z3 parallel.enable=true smt.threads=4 experiment/out_int.smt2 > experiment/result_int" in
        (* let _ = Sys.command "/usr/bin/time -v z3 experiment/out_int.smt2 > experiment/result_int" in *)
        let ic = open_in "experiment/result_int" in
        let first_line = input_line ic in
        (if first_line = "unknown" then 
          (
            (* Printf.printf "iter: %d unknown " !iter; *)
          (* let end_time = Unix.gettimeofday () in *)
          (* Printf.printf "time: %fs\n" (end_time -. start_time); *)
          flush stdout;
          add_sample ()
          )
        else if first_line = "unsat" then
          (Printf.printf "iter: %d unsat " !iter;
          let end_time = Unix.gettimeofday () in
          Printf.printf "time: %fs\nprogram error \n" (end_time -. start_time);
          flush stdout;
          continue := false;
          exit 0;
          )
        else if first_line = "sat" then
          (
            Printf.printf "int fin \n";
          flush stdout;
          main_fv file_name;
          let _ = Sys.command (Format.sprintf "z3 experiment/out_fv.smt2 > %s" result_path) in
          Printf.printf "fv fin ";
          let ic = open_in result_path in
          (* 最初の行を読み取る *)
          let first_line = input_line ic in
          (* ファイルを閉じる *)
          close_in ic;
          (* 比較して結果を出力 *)
          if first_line = "sat" then 
            (let end_time = Unix.gettimeofday () in
            Printf.printf "iter: %d sat own time: %fs\nownership: sat\n" !iter (end_time -. start_time);
            continue := false) 
          else 
            (
              Printf.printf "iter: %d unsat " !iter;
            let end_time = Unix.gettimeofday () in
            Printf.printf "time: %fs\n" (end_time -. start_time);
            (* main_cexample file_name; *)
            (* flush stdout;
            let _ = Sys.command "z3 experiment/out_cexample.smt2 > experiment/result_cexample" in
            flush stdout;
            (* create_cexapmle (); *)
            flush stdout; *)
            flush stdout;
            (* Printf.printf "cex fin "; *)
            ))
          else
            (Printf.printf "implement error\n %s\n" first_line;
            let end_time = Unix.gettimeofday () in
            Printf.printf "time: %fs\nprogram error \n" (end_time -. start_time);
            flush stdout;
            continue := false;
            exit 0;
            ));
        flush stdout;
        iter := !iter + 1       *)
      (* done; *)
    with Sys_error msg -> Printf.eprintf "Error: %s\n" msg
    | End_of_file -> Printf.printf "canceled"
    (* | _ -> 
      Printf.eprintf "Error: %s\n" "unsat";
      let end_time = Unix.gettimeofday () in
      Printf.printf "time: %fs\n" (end_time -. start_time) *)
      )

(* 実行モードを管理する型 *)
type run_mode = 
  | Normal
  | PrintProgram
  | Refinement
  | FullAnnotated

let () = 
  let file_name = ref "" in
  let mode = ref Normal in
  let insert_alias = ref false in
  let unsat_core = ref true in
  let random_assignment = ref false in
  let speclist = [
    (* -full_annotated が指定されたらモードを変更 *)
    ("-full_annotated", Arg.Unit (fun () -> mode := FullAnnotated), "Run with full annotations");
    (* ブール値のフラグ (スイッチ) *)
    ("-insert_alias", Arg.Set insert_alias, "Enable alias insertion");
    ("-random_assignment", Arg.Set random_assignment, "Enable random assignment");
    (* 値を取るオプション (例: -unsat-core true) *)
    ("-unsat-core", Arg.Bool (fun b -> unsat_core := b), "Enable/Disable unsat core (default: true)");
    (* その他のモード *)
    ("-print_program", Arg.Unit (fun () -> mode := PrintProgram), "Print the program");
    ("-refinement", Arg.Unit (fun () -> mode := Refinement), "Run refinement (already ownership inference complete)");
  ] in
  let usage_msg = "Usage: dune exec myproject -- <file> [options]" in

  (* 引数の解析実行 *)
  (* 匿名引数（フラグでないもの＝ファイル名）が来たら input_file に入れる *)
  Arg.parse speclist (fun input -> file_name := input) usage_msg;
  (* ファイル名が指定されていない場合のエラーハンドリング *)
  if !file_name = "" then (
    Arg.usage speclist usage_msg;
    exit 1
  );
  match !mode with
  | Normal -> 
    execute_main !file_name !unsat_core !insert_alias !random_assignment
  | FullAnnotated ->
    execute_main_annotated !file_name !unsat_core !insert_alias !random_assignment
  | PrintProgram ->
    print_program !file_name !insert_alias
  (* | [_; file_name; "full_annotated"] ->
    execute_main_annotated file_name *)
  (* | [_; file_name; option] when String.starts_with ~prefix:"unsat-core=" option ->
    (* "unsat-core=" の部分を除いた文字列を取得 *)
    let value_str = String.sub option 11 (String.length option - 11) in
    let unsat_core_enabled = (value_str = "true") in
    execute_main file_name unsat_core_enabled *)
  | Refinement ->
    let result_path = (Format.sprintf "experiment/own_result/result_%s" (Filename.basename !file_name)) in
    let start_time = Unix.gettimeofday () in
    main_chc !file_name result_path true !insert_alias;
    let _ = Sys.command "hoice experiment/out_chc.smt2 > experiment/chc_result" in
    let ic = open_in "experiment/chc_result" in
    (* 最初の行を読み取る *)
    let first_line = input_line ic in
    Printf.printf "refinement: %s\ntotal time: %fs\n" first_line (Unix.gettimeofday () -. start_time);
    flush stdout;
    (if first_line = "unsat" && !unsat_core
    then
      (
        (* main_chc file_name result_path true; *)
      let _ = Sys.command "z3 parallel.enable=true smt.threads=4 experiment/out_chc.smt2 > experiment/chc_result" in
      ())
    );
    flush stdout
  (* | _ -> Printf.eprintf "予期せぬエラーが発生しました\n" *)