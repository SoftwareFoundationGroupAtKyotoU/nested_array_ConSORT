open Syntax
open SimpleTyping

(* 単純型付けの情報をもとにプログラムの詳細化
Let束縛を分類したり代入とエイリアス注釈をデリファレンスか否かで場合わけ *)
let rec elaborate_exp fun_name exp =
  match exp with
  | Let (id,exp1,exp2) ->
    if lookup id (lookup fun_name !all_tyenv) = SInt then
      LetIntExp(id, elaborate_exp fun_name exp1,  elaborate_exp fun_name exp2)
    else 
      (match exp1 with
       | Var id1 -> LetAddPtrExp(id, id1, ILit 0, elaborate_exp fun_name exp2)
       | PlusExp(Var id1, e) -> LetAddPtrExp(id, id1, elaborate_exp fun_name e,  elaborate_exp fun_name exp2) 
       | MinusExp(Var id1, e) -> LetAddPtrExp(id, id1, elaborate_exp fun_name ( MinusExp(ILit 0 , e) ),  elaborate_exp fun_name exp2) 
       | Deref id1 -> LetDerefExp(id, id1,  elaborate_exp fun_name exp2)
       | _ -> err "ElaborateError")
  | Assign (id,exp1,exp2) ->
    if lookup id (lookup fun_name !all_tyenv) = SRef SInt then 
      AssignInt(id,  elaborate_exp fun_name exp1,  elaborate_exp fun_name exp2)
    else
      (match exp1 with
      | Var id1 -> AssignPtr(id, id1,  elaborate_exp fun_name exp2)
      | _ -> err "ElaborateError: If this error occurs, the simpleTyping is wrong.")
  | Alias (exp1,exp2,exp3) -> 
    (match exp1 with
    | Var id1 ->
      (match exp2 with
      | Var id2 -> AliasAddPtr(id1, id2, ILit 0, elaborate_exp fun_name exp3)
      | PlusExp (Var id2, exp) -> AliasAddPtr(id1, id2, exp,  elaborate_exp fun_name exp3)
      | MinusExp (Var id2, exp) -> AliasAddPtr(id2, id1, exp,  elaborate_exp fun_name exp3) 
      | Deref id2 -> AliasDeref(id1, id2,  elaborate_exp fun_name exp3)
      | _ -> err "ElaborateError")
    | _ -> err "ElaborateError: If this error occurs, the simpleTyping is wrong.")
  | IfExp (exp1,exp2,exp3) -> IfExp(elaborate_exp fun_name exp1,  elaborate_exp fun_name exp2,  elaborate_exp fun_name exp3)
  | IfnpExp (id,exp1,exp2) -> IfnpExp(id,  elaborate_exp fun_name exp1,  elaborate_exp fun_name exp2)
  | LetAllocExp (id,exp1,simpleTy,exp2) -> LetAllocExp(id, elaborate_exp fun_name exp1, simpleTy, elaborate_exp fun_name exp2)
  | Assert (exp1,exp2) -> Assert(elaborate_exp fun_name exp1,  elaborate_exp fun_name exp2)
  | Seq (exp1,exp2) -> Seq(elaborate_exp fun_name exp1,  elaborate_exp fun_name exp2)
  | AppExp (id,es) -> AppExp(id, List.map (elaborate_exp fun_name) es)
  | EqExp (exp1,exp2) -> EqExp(elaborate_exp fun_name exp1,  elaborate_exp fun_name exp2)
  | LtExp (exp1,exp2) -> LtExp(elaborate_exp fun_name exp1,  elaborate_exp fun_name exp2)
  | GtExp (exp1,exp2) -> GtExp(elaborate_exp fun_name exp1,  elaborate_exp fun_name exp2)
  | LeqExp (exp1,exp2) -> LeqExp(elaborate_exp fun_name exp1,  elaborate_exp fun_name exp2)
  | GeqExp (exp1,exp2) -> GeqExp(elaborate_exp fun_name exp1,  elaborate_exp fun_name exp2)
  | NeqExp (exp1,exp2) -> NeqExp(elaborate_exp fun_name exp1,  elaborate_exp fun_name exp2)
  | AndExp (exp1,exp2) -> AndExp(elaborate_exp fun_name exp1,  elaborate_exp fun_name exp2)
  | OrExp (exp1,exp2) -> OrExp(elaborate_exp fun_name exp1,  elaborate_exp fun_name exp2)
  | NotExp e -> NotExp(elaborate_exp fun_name e)
  | PlusExp (exp1,exp2) -> PlusExp(elaborate_exp fun_name exp1,  elaborate_exp fun_name exp2)
  | MinusExp (exp1,exp2) -> MinusExp(elaborate_exp fun_name exp1,  elaborate_exp fun_name exp2)
  | MultExp (exp1,exp2) -> MultExp(elaborate_exp fun_name exp1,  elaborate_exp fun_name exp2)
  | _ -> exp

let elaborate_prog prog = 
  let (fdefs, e) = prog in
  let fdefs' = List.map (fun (id,ids,ann,e) -> (id, ids, ann, elaborate_exp id e)) fdefs in
  let e' = elaborate_exp "main" e in
  (fdefs', e')