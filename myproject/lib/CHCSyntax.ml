open Syntax
open OwnConstraintSyntax
open SmtlibSyntax

let rec ifel_to_str ifel = 
  match ifel with
  | [] -> ""
  | s :: ifel' -> "_" ^ s ^ ifel_to_str ifel'

(* 篩型の環境
変数名と篩型内で依存できる変数集合 *)
let intpred_env : (id * id list) list ref = ref []

(** AST with position information used for refienment inference *)
type chc = 
  | CHCIf of exp * chc list * chc list * pos
  | CHCLetInt of id * exp * pos
  | CHCLetDeref of id * id * pos
  | CHCLetAddPtr of id * id * exp * pos
  | CHCAlloc of id * exp * simpleTy * pos
  | CHCAssignInt of id * exp * pos
  | CHCAssignRef of id * id * pos
  | CHCAlias of id * id * pos
  | CHCAliasDeref of id * id * pos
  | CHCAliasAddPtr of id * id * exp * pos
  | CHCAssert of exp * pos
  | CHCAssume of exp * chc list * pos


(* intpred_env:篩型の環境
num:関数の番号 *)
let print_declare_chc_int oc intpred_env num =
let intpred_set = PrintOwnConstraint.list_to_set intpred_env [] in
List.iter
  (fun (id,fvs) ->
     output_string oc (Format.sprintf "(declare-fun P%d_%s ( Int " num id);
     List.iter (fun _ -> output_string oc "Int ") fvs;
     output_string oc (") Bool)\n")
     ) intpred_set

(* id_count_chc: (変数id, (プログラムの位置l, ifel))のリスト *)
let print_declare_chc oc id_count num =
List.iter
  (fun (id,(pos,ifel,depth, fvs)) ->
     try 
       let fvs' = List.assoc id !intpred_env in
       output_string oc (Format.sprintf "(declare-fun P%d_%s ( " num id);
       for _ = 1 to depth do output_string oc "Int " done;
       List.iter (fun _ -> output_string oc "Int ") fvs';
       output_string oc (") Bool)\n")
     with Not_found -> 
       output_string oc (Format.sprintf 
       "(declare-fun P%d_%s_%d%s ( Int " num id pos (ifel_to_str ifel ));
       for _ = 1 to depth do output_string oc "Int " done;
       List.iter (fun _ -> output_string oc "Int ") fvs;
       output_string oc ") Bool)\n"
     ) id_count

(* varpred_count: 篩型の述語，(所有範囲の下限，所有範囲の上限，所有権の値)のリスト 
術後の記述*)
let print_declare_varpred oc varpred_count num =
 List.iter
   (fun sl ->
      match sl with
      | IntVarPred(num',id,fvs) -> 
        if num' = num then 
          (output_string oc (Format.sprintf"(declare-fun P%d_%s ( Int " num id);
          List.iter (fun _ -> output_string oc "Int ") fvs;
          output_string oc (") Bool)\n"))
        else ()
      | PtrVarPred(num',id,be,idx_list,fvs) -> 
        if num' = num then 
          (output_string oc (Format.sprintf "(declare-fun P%d_%s_%s ( Int " num id be);
        List.iter (fun _ -> output_string oc "Int ") idx_list;
        List.iter (fun _ -> output_string oc "Int ") fvs;
        output_string oc (") Bool)\n"))
        else ()
      | _ -> raise (Error "print_declare_varpred error")
        ) (PrintOwnConstraint.list_to_set (List.map fst varpred_count) [])