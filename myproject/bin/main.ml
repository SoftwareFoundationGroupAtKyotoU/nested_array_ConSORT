open Myproject.Output

let () = 
  match Array.to_list Sys.argv with
    [_; file_name] -> 
    (try 
      generate_constrs file_name 5
      (* read_eval_print initial_env input *)
    with Sys_error msg -> Printf.eprintf "Error: %s\n" msg)
  | _ -> Printf.eprintf "予期せぬエラーが発生しました"