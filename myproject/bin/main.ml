open Myproject.Output
open Myproject.Cexample

let () = 
  let start_time = Unix.gettimeofday () in
  match Array.to_list Sys.argv with
    [_; file_name] -> 
    (try 
      let iter = ref 0 in
      let continue = ref true in
      while !continue do
        generate_constrs file_name !iter;
        let _ = Sys.command "z3 experiment/out_int.smt2 > experiment/result_int" in
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
          Printf.printf "time: %fs\nprogram error" (end_time -. start_time);
          continue := false
          )
        else if first_line = "sat" then
          (Printf.printf "int fin ";
          (* Printf.printf "%d\n" exit_code; *)
          main_fv file_name;
          let _ = Sys.command "z3 experiment/out_fv.smt2 > experiment/result" in
          Printf.printf "fv fin ";
          let ic = open_in "experiment/result" in
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
            let _ = Sys.command "z3 experiment/out_cexample.smt2 > experiment/result_cexample" in
            create_cexapmle ();
            Printf.printf "cex fin ";
            )));
        flush stdout;
        iter := !iter + 1      
      done;
      main_sat_ans file_name
      (* read_eval_print initial_env input *)
    with Sys_error msg -> Printf.eprintf "Error: %s\n" msg
    | _ -> 
      Printf.eprintf "Error: %s\n" "unsat";
      let end_time = Unix.gettimeofday () in
      Printf.printf "time: %fs\n" (end_time -. start_time))
  | [_; file_name; "print_program"] ->
    print_program file_name
  | [_; file_name; iter] ->
    generate_constrs file_name (int_of_string iter)
  | _ -> Printf.eprintf "予期せぬエラーが発生しました"