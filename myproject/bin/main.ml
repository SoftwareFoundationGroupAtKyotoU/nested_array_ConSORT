open Myproject.Output
open Myproject.Cexample

let () = 
  let start_time = Unix.gettimeofday () in
  match Array.to_list Sys.argv with
    [_; file_name] -> 
    (try 
      let iter = ref 0 in
      let continue = ref true in
      let oc = open_out "experiment/result_cex_all" in
      output_string oc "counter example list\n";
      let result_path = (Format.sprintf "experiment/own_result/result_%s" (Filename.basename file_name)) in
      let oc = open_out result_path in
      close_out oc;
      flush oc;
      close_out oc;
      while !continue do
        generate_constrs file_name;
        let _ = Sys.command "z3 parallel.enable=true smt.threads=4 experiment/out_int.smt2 > experiment/result_int" in
        (* let _ = Sys.command "/usr/bin/time -v z3 experiment/out_int.smt2 > experiment/result_int" in *)
        let ic = open_in "experiment/result_int" in
        let first_line = input_line ic in
        (if first_line = "unknown" then 
          (Printf.printf "iter: %d unknown " !iter;
          let end_time = Unix.gettimeofday () in
          Printf.printf "time: %fs\n" (end_time -. start_time);
          add_sample ()
          )
        else if first_line = "unsat" then
          (Printf.printf "iter: %d unsat " !iter;
          let end_time = Unix.gettimeofday () in
          Printf.printf "time: %fs\nprogram error " (end_time -. start_time);
          continue := false
          )
        else if first_line = "sat" then
          (Printf.printf "int fin \n";
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
            Printf.printf "iter: %d sat time: %fs\nownership: sat\n" !iter (end_time -. start_time);
            continue := false) 
          else 
            (Printf.printf "iter: %d unsat " !iter;
            let end_time = Unix.gettimeofday () in
            Printf.printf "time: %fs\n" (end_time -. start_time);
            main_cexample file_name;
            flush stdout;
            let _ = Sys.command "z3 experiment/out_cexample.smt2 > experiment/result_cexample" in
            flush stdout;
            create_cexapmle ();
            flush stdout;
            Printf.printf "cex fin ";
            )));
        flush stdout;
        iter := !iter + 1      
      done;
      (main_sat_ans file_name;
      main_chc file_name result_path false;
      let _ = Sys.command "hoice experiment/out_chc.smt2 > experiment/chc_result" in
      let ic = open_in "experiment/chc_result" in
      (* 最初の行を読み取る *)
      let first_line = input_line ic in
      Printf.printf "refinement: %s\ntotal time: %fs\n" first_line (Unix.gettimeofday () -. start_time);
      flush stdout;
      (if first_line = "unsat" then
        (main_chc file_name result_path true;
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
  | [_; file_name; "print_program"] ->
    print_program file_name
  | [_; file_name; "refinement"] ->
    let result_path = (Format.sprintf "experiment/own_result/result_%s" (Filename.basename file_name)) in
    let start_time = Unix.gettimeofday () in
    main_chc file_name result_path false;
    let _ = Sys.command "hoice experiment/out_chc.smt2 > experiment/chc_result" in
    let ic = open_in "experiment/chc_result" in
    (* 最初の行を読み取る *)
    let first_line = input_line ic in
    Printf.printf "refinement: %s\ntotal time: %fs\n" first_line (Unix.gettimeofday () -. start_time);
    flush stdout;
    (if first_line = "unsat" 
    then
      (main_chc file_name result_path true;
      let _ = Sys.command "z3 parallel.enable=true smt.threads=4 experiment/out_chc.smt2 > experiment/chc_result" in
      ())
    );
    flush stdout
  | _ -> Printf.eprintf "予期せぬエラーが発生しました"