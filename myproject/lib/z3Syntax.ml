(** Module for handling the model outputtted by Z3 *)

type id = string

type z3_type = string

(** Values assigned to variables *)
type value =
  | Int of int
  | Float of float
  | Div of float * float

let pp_value fmt value =
  match value with
  | Int i -> Format.fprintf fmt "%d" i
  | Float i -> Format.fprintf fmt "%f" i
  | Div (f1,f2) -> Format.fprintf fmt "%f/%f" f1 f2
  

type define = id * z3_type * value

type result = define list

let serial_num = ref 0

let rec print_z3result oc z3res = 
  List.iter (print_z3result_sub oc) z3res
and print_z3result_sub oc def = 
  match def with
  | (id, _,Int i) ->
    (output_string oc "(assert (! (= " ;
     output_string oc (id ^ " ");
     output_string oc (string_of_int i);
     output_string oc (") :named val" ^ (string_of_int !serial_num) ^ "))\n");
     serial_num := !serial_num + 1)
  | (id,_,Float f) ->
    (output_string oc "(assert (! (= ";
     output_string oc (id ^ " ");
     output_string oc (string_of_float f);
     output_string oc (") :named val" ^ (string_of_int !serial_num) ^ "))\n");
     serial_num := !serial_num + 1)
  | (id,_,Div (f1,f2)) ->
    (output_string oc "(assert (! (= ";
     output_string oc (id ^ " (/ ");
     output_string oc (string_of_float f1);
     output_string oc " ";
     output_string oc (string_of_float f2);
     output_string oc (")) :named val" ^ (string_of_int !serial_num) ^ "))\n");
     serial_num := !serial_num + 1)

(* z3の結果から該当idのものを探して答えを出力する関数 *)
let rec print_z3result_value oc z3_res id = 
  match z3_res with
  | [] -> output_string oc id
  | (id', _,Int i) :: _ when id = id' ->
    (output_string oc (string_of_int i);)
  | (id',_,Float f) :: _ when id = id' ->
    (output_string oc (string_of_float f);)
  | (id',_,Div (f1,f2))  :: _ when id = id' ->
    ( output_string oc (" (/ ");
      output_string oc (string_of_float f1);
      output_string oc " ";
      output_string oc (string_of_float f2);
      output_string oc (")");
      )
  | _ :: left -> print_z3result_value oc left id
