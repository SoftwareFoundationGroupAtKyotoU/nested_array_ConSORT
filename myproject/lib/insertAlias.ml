open Syntax

let rec insert_sub alias aliasId exp =
  match exp with
  | Let (id,e1,e2) -> Let(id, e1, insert_sub alias aliasId e2)
  | LetIntExp(id,e1,e2) -> LetIntExp(id, e1, insert_sub alias aliasId e2)
  | LetDerefExp (id1,id2,e) -> LetDerefExp(id1, id2, insert_sub alias aliasId e)
  | LetAddPtrExp (id1,id2,e1,e2) -> LetAddPtrExp(id1, id2, e1, insert_sub alias aliasId e2)
  (* | LetAddPtrExp (id1,id2,e1,e2) when id2 != aliasId -> LetAddPtrExp(id1, id2, e1, insert_sub alias aliasId e2)
  | LetAddPtrExp _ -> alias exp *)
  | LetImmutAddPtrExp (id1,id2,e1,e2) -> LetImmutAddPtrExp(id1, id2, e1, insert_sub alias aliasId e2)
  | IfExp (e1,e2,e3) -> IfExp(e1, insert_sub alias aliasId e2, insert_sub alias aliasId e3)
  | IfnpExp (e1,e2,e3) -> IfnpExp(e1, insert_sub alias aliasId e2, insert_sub alias aliasId e3)
  | LetAllocExp (id,e1, ty ,e2) -> LetAllocExp(id, e1,ty, insert_sub alias aliasId e2)
  | Assign(id,e1,e2) -> Assign (id, e1, insert_sub alias aliasId e2)
  | AssignInt(id,e1,e2) -> AssignInt (id, e1, insert_sub alias aliasId e2)
  | AssignPtr(id1,id2,e) -> AssignPtr (id1, id2, insert_sub alias aliasId e)
  | Assert (e1,e2) -> Assert(e1, insert_sub alias aliasId e2)
  | Assume (e1,e2) -> Assume(e1, insert_sub alias aliasId e2)
  | Seq (e1,e2) -> Seq(e1, insert_sub alias aliasId e2)
  | AliasAddPtr (id1, id2, e1, e2) -> AliasAddPtr (id1, id2, e1, insert_sub alias aliasId e2)
  | AliasDeref(id1,id2,e) -> AliasDeref(id1, id2, insert_sub alias aliasId e)
  | _ -> alias exp

let rec insert_alias_body exp = 
  match exp with
  | LetDerefExp (id1,id2,e) ->
    let alias e' = AliasDeref(id1,id2,e') in 
    LetDerefExp(id1, id2, insert_sub alias id2 (insert_alias_body e))
  | LetAddPtrExp (id1,id2,e1,e2) -> 
    let alias e' = AliasAddPtr(id1,id2,e1,e') in 
    LetAddPtrExp(id1, id2, e1, insert_sub alias id2 (insert_alias_body e2))
  | LetImmutAddPtrExp (id1,id2,e1,e2) -> 
    let alias e' = AliasAddPtr(id1,id2,e1,e') in 
    LetImmutAddPtrExp(id1, id2, e1, insert_sub alias id2 (insert_alias_body e2))
  | Let (id,e1,e2) -> Let (id, insert_alias_body e1, insert_alias_body e2)
  | LetIntExp(id,e1,e2) -> LetIntExp (id, insert_alias_body e1, insert_alias_body e2)
  | IfExp (e1,e2,e3) -> IfExp(insert_alias_body e1, insert_alias_body e2, insert_alias_body e3)
  | IfnpExp (id,e2,e3) -> IfnpExp(id, insert_alias_body e2, insert_alias_body e3)
  | LetAllocExp (id,e1,ty,e2) -> LetAllocExp(id,e1,ty, insert_alias_body e2)
  | Assign(id,e1,e2) -> Assign (id, insert_alias_body e1, insert_alias_body e2)
  | AssignInt(id,e1,e2) -> AssignInt (id, insert_alias_body e1, insert_alias_body e2)
  | AssignPtr(id1,id2,e) -> AssignPtr (id1, id2, insert_alias_body e)
  | Assert (e1,e2) -> Assert(insert_alias_body e1, insert_alias_body e2)
  | Assume (e1,e2) -> Assume(insert_alias_body e1, insert_alias_body e2)
  | Seq (e1,e2) -> Seq(insert_alias_body e1, insert_alias_body e2)
  | AppExp (id,es) -> AppExp(id, List.map insert_alias_body es)
  | EqExp (e1,e2) -> EqExp(insert_alias_body e1, insert_alias_body e2)
  | LtExp (e1,e2) -> LtExp(insert_alias_body e1, insert_alias_body e2)
  | GtExp (e1,e2) -> GtExp(insert_alias_body e1, insert_alias_body e2)
  | LeqExp (e1,e2) -> LeqExp(insert_alias_body e1, insert_alias_body e2)
  | GeqExp (e1,e2) -> GeqExp(insert_alias_body e1, insert_alias_body e2)
  | NeqExp (e1,e2) -> NeqExp(insert_alias_body e1, insert_alias_body e2)
  | AndExp (e1,e2) -> AndExp(insert_alias_body e1, insert_alias_body e2)
  | OrExp (e1,e2) -> OrExp(insert_alias_body e1, insert_alias_body e2)
  | NotExp e -> NotExp(insert_alias_body e)
  | PlusExp (e1,e2) -> PlusExp(insert_alias_body e1, insert_alias_body e2)
  | MinusExp (e1,e2) -> MinusExp(insert_alias_body e1, insert_alias_body e2)
  | MultExp (e1,e2) -> MultExp(insert_alias_body e1, insert_alias_body e2)
  | DivExp (e1,e2) -> DivExp(insert_alias_body e1, insert_alias_body e2)
  | _ -> exp

let insert_alias prog = 
  let (fdefs, e) = prog in
  let fdefs' = List.map (fun (id,ids,ann,e) -> (id, ids, ann, insert_alias_body e)) fdefs in
  let e' = insert_alias_body e in
  (* if flag then  *)
    (fdefs', e')
  (* else
    (fdefs, e) *)