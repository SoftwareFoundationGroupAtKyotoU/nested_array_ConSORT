open Myproject.Output

let () = match Array.to_list Sys.argv with
    [_; file_name] -> 
    (try 
      (*ファイルを開いて読み取る*)
      let input = open_in file_name in
      read_eval_print initial_env initial_tyenv input
    with Sys_error msg -> Printf.eprintf "Error: %s\n" msg)
  | _ -> Printf.eprintf "予期せぬエラーが発生しました"