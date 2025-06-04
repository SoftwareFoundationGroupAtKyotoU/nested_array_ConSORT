(** An intermediate module to pass the ownership information obtained to the refinment inference phase *)

open Z3Syntax2
open Syntax
open SmtlibSyntax
open Util
open PrintOwnConstraint
open OwnConstraintSyntax
open CHCSyntax

(* 代入，読み出しにより変則的な所有権の形をしているidのリスト *)
let eq0_list : id list ref = ref []

let make_idx_list depth =
  let rec make_idx_list_sub depth lis =
    if depth <= 0 then lis else
      make_idx_list_sub (depth-1) ((FV (Format.sprintf "i%n" depth))::lis) in
  make_idx_list_sub depth []

(* num番目の関数のz3からの所有権推論結果から所有権を持つ変数名と関数内でのその変数の位置をリストアップ *)
let rec get_id ownerships num = 
  match ownerships with
  | Own (num',id,i,_,_) :: rest when num = num' -> (id, i) :: get_id rest num
  | _ :: rest -> get_id rest num
  | [] -> []

let find_ref_ids num ownerships =
  let rec find_ref_ids_sub ownerships res =
    match ownerships with
    | Own (num',id1,_,_,_) :: rest when num = num' && not (List.mem id1 res) -> find_ref_ids_sub rest (id1::res)
    | _ :: rest -> find_ref_ids_sub rest res
    | [] -> res
  in find_ref_ids_sub ownerships []

(* 関数の番号num, 変数名id, 関数内での位置iに一致するz3からの所有権推論結果を抽出する *)
let rec find_id (id,i) num ownerships =
  match ownerships with
  (* num':関数の順番番号 id1:どの変数の所有権を表すか　i1:関数内の場所 f:所有権？ *)
  | Own (num',id1,i1,depth,Float f) :: rest when num = num' && id = id1 && i = i1 -> Own (num',id1,i1,depth,Float f) :: find_id (id,i) num rest
  (* CHigh:所有範囲の上限を表す一次式の係数　num':関数の順番番号？ id1:どの変数の係数となるか　id2:どの変数の所有権を表すか　i1:関数内の場所 i2:係数の数値？ *)
  | CHigh (num',id1,id2,i1,depth,Int i2) :: rest when num = num' && id = id2 && i = i1 -> CHigh (num',id1,id2,i1,depth,Int i2) :: find_id (id,i) num rest
  (* CHigh:所有範囲の下限を表す一次式の係数　num':関数の順番番号？ id1:どの変数の係数となるか　id2:どの変数の所有権を表すか　i1:関数内の場所 i2:係数の数値？ *)
  | CLow (num',id1,id2,i1,depth,Int i2) :: rest when num = num' && id = id2 && i = i1 -> CLow (num',id1,id2,i1,depth,Int i2) :: find_id (id,i) num rest
  (* DHigh:所有範囲の上限を表す一次式の切片　num':関数の順番番号？　id1:どの変数の所有権を表すか　i1:関数内の場所 i2:切片の数値？ *)
  | DHigh (num',id1,i1,depth,Int i2) :: rest when num = num' && id = id1 && i = i1 -> DHigh (num',id1,i1,depth,Int i2) :: find_id (id,i) num rest
  (* DHigh:所有範囲の下限を表す一次式の切片　num':関数の順番番号？ id1:どの変数の所有権を表すか　i1:関数内の場所 i2:切片の数値？ *)
  | DLow (num',id1,i1,depth,Int i2) :: rest when num = num' && id = id1 && i = i1 -> DLow (num',id1,i1,depth,Int i2) :: find_id (id,i) num rest
  | _ :: rest -> find_id (id,i) num rest
  | [] -> []

(* let rec find_id_d (id,i) num ownerships dep =
  match ownerships with
  (* num':関数の順番番号 id1:どの変数の所有権を表すか　i1:関数内の場所 f:所有権？ *)
  | Own (num',id1,i1,depth,Float f) :: rest when num = num' && id = id1 && i = i1 && dep = depth -> Own (num',id1,i1,depth,Float f) :: find_id_d (id,i) num rest dep
  (* CHigh:所有範囲の上限を表す一次式の係数　num':関数の順番番号？ id1:どの変数の係数となるか　id2:どの変数の所有権を表すか　i1:関数内の場所 i2:係数の数値？ *)
  | CHigh (num',id1,id2,i1,depth,Int i2) :: rest when num = num' && id = id2 && i = i1 && dep = depth -> CHigh (num',id1,id2,i1,depth,Int i2) :: find_id_d (id,i) num rest dep
  (* CHigh:所有範囲の下限を表す一次式の係数　num':関数の順番番号？ id1:どの変数の係数となるか　id2:どの変数の所有権を表すか　i1:関数内の場所 i2:係数の数値？ *)
  | CLow (num',id1,id2,i1,depth,Int i2) :: rest when num = num' && id = id2 && i = i1 && dep = depth -> CLow (num',id1,id2,i1,depth,Int i2) :: find_id_d (id,i) num rest dep
  (* DHigh:所有範囲の上限を表す一次式の切片　num':関数の順番番号？　id1:どの変数の所有権を表すか　i1:関数内の場所 i2:切片の数値？ *)
  | DHigh (num',id1,i1,depth,Int i2) :: rest when num = num' && id = id1 && i = i1 && dep = depth -> DHigh (num',id1,i1,depth,Int i2) :: find_id_d (id,i) num rest dep
  (* DHigh:所有範囲の下限を表す一次式の切片　num':関数の順番番号？ id1:どの変数の所有権を表すか　i1:関数内の場所 i2:切片の数値？ *)
  | DLow (num',id1,i1,depth,Int i2) :: rest when num = num' && id = id1 && i = i1 && dep = depth -> DLow (num',id1,i1,depth,Int i2) :: find_id_d (id,i) num rest dep
  | _ :: rest -> find_id_d (id,i) num rest dep
  | [] -> [] *)

let rec is_0own (id,i) ownerships =
  match ownerships with
  (* num':関数の順番番号 id1:どの変数の所有権を表すか　i1:関数内の場所 f:所有権？ *)
  | Own (_,id1,i1,_,Float f) :: _ when id = id1 && i = i1 && f = 0.
  -> true
  | _ :: rest -> is_0own (id,i) rest
  | [] -> false

let max_depth ownerships =
  let rec max_depth_sub ownerships res =
    match ownerships with
    (* num':関数の順番番号 id1:どの変数の所有権を表すか　i1:関数内の場所 f:所有権？ *)
    | Own (_, _, _,depth, _) :: rest when res < depth -> max_depth_sub rest depth
    (* CHigh:所有範囲の上限を表す一次式の係数　num':関数の順番番号？ id1:どの変数の係数となるか　id2:どの変数の所有権を表すか　i1:関数内の場所 i2:係数の数値？ *)
    | CHigh (_, _, _, _,depth,_) :: rest when res < depth -> max_depth_sub rest depth
    (* CHigh:所有範囲の下限を表す一次式の係数　num':関数の順番番号？ id1:どの変数の係数となるか　id2:どの変数の所有権を表すか　i1:関数内の場所 i2:係数の数値？ *)
    | CLow (_, _, _, _,depth,_) :: rest when res < depth -> max_depth_sub rest depth
    (* DHigh:所有範囲の上限を表す一次式の切片　num':関数の順番番号？　id1:どの変数の所有権を表すか　i1:関数内の場所 i2:切片の数値？ *)
    | DHigh (_, _, _,depth,_) :: rest when res < depth -> max_depth_sub rest depth
    (* DHigh:所有範囲の下限を表す一次式の切片　num':関数の順番番号？ id1:どの変数の所有権を表すか　i1:関数内の場所 i2:切片の数値？ *)
    | DLow (_, _, _,depth,_) :: rest when res < depth -> max_depth_sub rest depth
    | _ :: rest -> max_depth_sub rest res
    | [] -> res in
  max_depth_sub ownerships 0

(** Main procedure: Adds constraints for Empty, i.e. types with ownership 0 *)
let rec own_to_chc (id,pos) full_ownerships sl =
  let max_dep = max_depth full_ownerships in
  if is_0own (id,pos) full_ownerships 
  then [Imply(Id "true", sl)] 
  else
    let rec own_to_chc_sub h_now l_now ownerships now_depth =
      if now_depth <= 0 then [True] 
      else
        match ownerships with
        | CHigh (_,id1,_,_,depth,Int i2) :: rest when depth = now_depth->
          (if i2 = Z.zero then
            (* 係数が0の場合は何もしない *)
            own_to_chc_sub h_now l_now rest now_depth
          else if i2 > Z.zero then
            (* 係数が0以外の場合はかけるべき変数とかけ合わせ足す *)
            own_to_chc_sub (Add(h_now, Mul(FV id1, Id (Z.to_string i2)))) l_now rest now_depth
          else
            own_to_chc_sub (Add(h_now, Mul(FV id1, Id ("(- " ^ Z.to_string (Z.neg i2) ^ ")")))) l_now rest now_depth)
        | CLow (_,id1,_,_,depth,Int i2) :: rest when depth = now_depth->
          (* 以下CHighと同じ *)
          (if i2 = Z.zero then
            own_to_chc_sub h_now l_now rest now_depth
          else if i2 > Z.zero then 
            own_to_chc_sub h_now (Add(l_now, Mul(FV id1, Id (Z.to_string i2)))) rest now_depth
          else 
            own_to_chc_sub h_now (Add(l_now, Mul(FV id1, Id ("(- " ^ Z.to_string (Z.neg i2) ^ ")")))) rest now_depth)
        | DHigh (_,_,_,depth,Int i2) :: rest when depth = now_depth->
          (if i2 = Z.zero then
            own_to_chc_sub h_now l_now rest now_depth
          else if i2 > Z.zero then
            own_to_chc_sub (Add(h_now, Id (Z.to_string i2))) l_now rest now_depth
          else
            own_to_chc_sub (Add(h_now, Id ("(- " ^ Z.to_string (Z.neg i2) ^ ")"))) l_now rest now_depth)
        | DLow (_,_,_,depth,Int i2) :: rest when depth = now_depth->
          (if i2 = Z.zero then
            own_to_chc_sub h_now l_now rest now_depth
          else if i2 > Z.zero then
            own_to_chc_sub h_now (Add(l_now, Id (Z.to_string i2))) rest now_depth
          else 
            own_to_chc_sub h_now (Add(l_now, Id ("(- " ^ Z.to_string (Z.neg i2) ^ ")"))) rest now_depth)
        | _ :: rest ->
          own_to_chc_sub h_now l_now rest now_depth
        | [] -> 
          (* 配列のインデックスが所有範囲の上限より大きい　ならば　篩型の述語？
            配列のインデックスが所有範囲の下限より小さい　ならば　篩型の述語？ *)
          if now_depth = 1 then
            [Imply(Gt(Id "i1", h_now), sl);
            Imply(Lt(Id "i1", l_now), sl)]
          else
            let sl1 =
              [Imply(Gt(Id (Format.sprintf "i%n" now_depth), h_now), sl);
              Imply(Lt(Id (Format.sprintf "i%n" now_depth), l_now), sl)] in
            let sl2 = own_to_chc_sub (Id "0") (Id "0") full_ownerships (now_depth-1) in
            sl1 @ (List.map (fun x -> Imply((And(Leq(Id (Format.sprintf "i%n" now_depth), h_now), Geq(Id (Format.sprintf "i%n" now_depth), l_now))), x)) sl2) in
    own_to_chc_sub (Id "0") (Id "0") full_ownerships max_dep

let rec own_to_chc_eq0 (id,pos) full_ownerships sl =
  let max_dep = max_depth full_ownerships in
  if is_0own (id,pos) full_ownerships 
  then [Imply(Id "true", sl)] 
  else
    let rec own_to_chc_sub h_now l_now ownerships now_depth =
      if now_depth <= 0 then [True] 
      else
        match ownerships with
        | CHigh (_,id1,_,_,depth,Int i2) :: rest when depth = now_depth->
          (if i2 = Z.zero then
            (* 係数が0の場合は何もしない *)
            own_to_chc_sub h_now l_now rest now_depth
          else if i2 > Z.zero then
            (* 係数が0以外の場合はかけるべき変数とかけ合わせ足す *)
            own_to_chc_sub (Add(h_now, Mul(FV id1, Id (Z.to_string i2)))) l_now rest now_depth
          else
            own_to_chc_sub (Add(h_now, Mul(FV id1, Id ("(- " ^ Z.to_string (Z.neg i2) ^ ")")))) l_now rest now_depth)
        | CLow (_,id1,_,_,depth,Int i2) :: rest when depth = now_depth->
          (* 以下CHighと同じ *)
          (if i2 = Z.zero then
            own_to_chc_sub h_now l_now rest now_depth
          else if i2 > Z.zero then 
            own_to_chc_sub h_now (Add(l_now, Mul(FV id1, Id (Z.to_string i2)))) rest now_depth
          else 
            own_to_chc_sub h_now (Add(l_now, Mul(FV id1, Id ("(- " ^ Z.to_string (Z.neg i2) ^ ")")))) rest now_depth)
        | DHigh (_,_,_,depth,Int i2) :: rest when depth = now_depth->
          (if i2 = Z.zero then
            own_to_chc_sub h_now l_now rest now_depth
          else if i2 > Z.zero then
            own_to_chc_sub (Add(h_now, Id (Z.to_string i2))) l_now rest now_depth
          else
            own_to_chc_sub (Add(h_now, Id ("(- " ^ Z.to_string (Z.neg i2) ^ ")"))) l_now rest now_depth)
        | DLow (_,_,_,depth,Int i2) :: rest when depth = now_depth->
          (if i2 = Z.zero then
            own_to_chc_sub h_now l_now rest now_depth
          else if i2 > Z.zero then
            own_to_chc_sub h_now (Add(l_now, Id (Z.to_string i2))) rest now_depth
          else 
            own_to_chc_sub h_now (Add(l_now, Id ("(- " ^ Z.to_string (Z.neg i2) ^ ")"))) rest now_depth)
        | _ :: rest ->
          own_to_chc_sub h_now l_now rest now_depth
        | [] -> 
          (* 配列のインデックスが所有範囲の上限より大きい　ならば　篩型の述語？
            配列のインデックスが所有範囲の下限より小さい　ならば　篩型の述語？ *)
          if now_depth = 1 then
            [Imply(Gt(Id "i1", h_now), sl);
            Imply(Lt(Id "i1", l_now), sl)]
          else
            let sl1 =
              [Imply(Gt(Id (Format.sprintf "i%n" now_depth), h_now), sl);
              Imply(Lt(Id (Format.sprintf "i%n" now_depth), l_now), sl)] in
            let sl2 = own_to_chc_sub (Id "0") (Id "0") full_ownerships (now_depth-1) in
            sl1 @ (List.map (fun x -> Imply((And(Leq(Id (Format.sprintf "i%n" now_depth), h_now), Geq(Id (Format.sprintf "i%n" now_depth), l_now))), x)) sl2) in
  let own_to_chc_sub_first now_depth =
    let sl1 = [Imply(Lt(Id (Format.sprintf "i%n" now_depth), Id "0"), sl)] in
    let sl2 = own_to_chc_sub (Id "0") (Id "0") full_ownerships (now_depth-1) in
    sl1 @ (List.map (fun x -> Imply(Eq(Id (Format.sprintf "i%n" now_depth), Id "0"), x)) sl2) in
  own_to_chc_sub_first max_dep

let rec own_to_chc_non0 (id,pos) full_ownerships sl =
  let max_dep = max_depth full_ownerships in
  if is_0own (id,pos) full_ownerships 
  then [Imply(Id "true", sl)] 
  else
    let rec own_to_chc_sub h_now l_now ownerships now_depth =
      if now_depth <= 0 then [True] 
      else
        match ownerships with
        | CHigh (_,id1,_,_,depth,Int i2) :: rest when depth = now_depth->
          (if i2 = Z.zero then
            (* 係数が0の場合は何もしない *)
            own_to_chc_sub h_now l_now rest now_depth
          else if i2 > Z.zero then
            (* 係数が0以外の場合はかけるべき変数とかけ合わせ足す *)
            own_to_chc_sub (Add(h_now, Mul(FV id1, Id (Z.to_string i2)))) l_now rest now_depth
          else
            own_to_chc_sub (Add(h_now, Mul(FV id1, Id ("(- " ^ Z.to_string (Z.neg i2) ^ ")")))) l_now rest now_depth)
        | CLow (_,id1,_,_,depth,Int i2) :: rest when depth = now_depth->
          (* 以下CHighと同じ *)
          (if i2 = Z.zero then
            own_to_chc_sub h_now l_now rest now_depth
          else if i2 > Z.zero then 
            own_to_chc_sub h_now (Add(l_now, Mul(FV id1, Id (Z.to_string i2)))) rest now_depth
          else 
            own_to_chc_sub h_now (Add(l_now, Mul(FV id1, Id ("(- " ^ Z.to_string (Z.neg i2) ^ ")")))) rest now_depth)
        | DHigh (_,_,_,depth,Int i2) :: rest when depth = now_depth->
          (if i2 = Z.zero then
            own_to_chc_sub h_now l_now rest now_depth
          else if i2 > Z.zero then
            own_to_chc_sub (Add(h_now, Id (Z.to_string i2))) l_now rest now_depth
          else
            own_to_chc_sub (Add(h_now, Id ("(- " ^ Z.to_string (Z.neg i2) ^ ")"))) l_now rest now_depth)
        | DLow (_,_,_,depth,Int i2) :: rest when depth = now_depth->
          (if i2 = Z.zero then
            own_to_chc_sub h_now l_now rest now_depth
          else if i2 > Z.zero then
            own_to_chc_sub h_now (Add(l_now, Id (Z.to_string i2))) rest now_depth
          else 
            own_to_chc_sub h_now (Add(l_now, Id ("(- " ^ Z.to_string (Z.neg i2) ^ ")"))) rest now_depth)
        | _ :: rest ->
          own_to_chc_sub h_now l_now rest now_depth
        | [] -> 
          (* 配列のインデックスが所有範囲の上限より大きい　ならば　篩型の述語？
            配列のインデックスが所有範囲の下限より小さい　ならば　篩型の述語？ *)
          if now_depth = 1 then
            [Imply(Gt(Id "i1", h_now), sl);
            Imply(Lt(Id "i1", l_now), sl)]
          else
            let sl1 =
              [Imply(Gt(Id (Format.sprintf "i%n" now_depth), h_now), sl);
              Imply(Lt(Id (Format.sprintf "i%n" now_depth), l_now), sl)] in
            let sl2 = own_to_chc_sub (Id "0") (Id "0") full_ownerships (now_depth-1) in
            sl1 @ (List.map (fun x -> Imply((And(Leq(Id (Format.sprintf "i%n" now_depth), h_now), Geq(Id (Format.sprintf "i%n" now_depth), l_now))), x)) sl2) in
  let rec own_to_chc_sub_first h_now ownerships now_depth =
    match ownerships with
    | CHigh (_,id1,_,_,depth,Int i2) :: rest when depth = now_depth->
      (if i2 = Z.zero then
        (* 係数が0の場合は何もしない *)
        own_to_chc_sub_first h_now rest now_depth
      else if i2 > Z.zero then
        (* 係数が0以外の場合はかけるべき変数とかけ合わせ足す *)
        own_to_chc_sub_first (Add(h_now, Mul(FV id1, Id (Z.to_string i2)))) rest now_depth
      else
        own_to_chc_sub_first (Add(h_now, Mul(FV id1, Id ("(- " ^ Z.to_string (Z.neg i2) ^ ")")))) rest now_depth)
    | DHigh (_,_,_,depth,Int i2) :: rest when depth = now_depth->
      (if Z.add Z.one i2 = Z.zero then
        own_to_chc_sub_first h_now rest now_depth
      else if Z.add Z.one i2 > Z.zero then
        own_to_chc_sub_first (Add(h_now, Id (Z.to_string (Z.add Z.one i2)))) rest now_depth
      else
        own_to_chc_sub_first (Add(h_now, Id ("(- " ^ Z.to_string (Z.neg (Z.add Z.one i2)) ^ ")"))) rest now_depth)
    | _ :: rest ->
      own_to_chc_sub_first h_now rest now_depth
    | [] -> 
      (* 一番外の配列の，補正を考慮したchcの制約 *)
      let sl1 = [Imply(Gt(Id (Format.sprintf "i%n" now_depth), h_now), sl)] in
      let sl2 = own_to_chc_sub (Id "0") (Id "0") full_ownerships (now_depth-1) in
      sl1 @ (List.map (fun x -> Imply((And(Leq(Id (Format.sprintf "i%n" now_depth), h_now), Geq(Id (Format.sprintf "i%n" now_depth), Id "1"))), x)) sl2) in
  own_to_chc_sub_first (Id "0") full_ownerships max_dep

let outer_constrs ownerships num fvs cons =
  let ref_ids = find_ref_ids num ownerships in
  let sl_first = List.concat_map (fun id -> 
    let id_ownerships = find_id (id,"1") num ownerships in
    let depth = max_depth id_ownerships in
    own_to_chc (id, "1") id_ownerships (PtrPred(id, "1", make_idx_list depth, fvs)))
    ref_ids in
  let rec outer_constrs_iter ifel cons =
    match cons with
    | CHCIf (_, c_lis1, c_lis2, pos) ->
      let pos_then = (string_of_int pos) ^ (ifel_to_str ("then" :: ifel)) in
      let pos_else = (string_of_int pos) ^ (ifel_to_str ("else" :: ifel)) in
      let pos_postif = (string_of_int (pos+1)) ^ (ifel_to_str ifel) in
      let make_sl pos = List.concat_map (fun id -> 
        let id_ownerships = find_id (id,pos) num ownerships in
        let depth = max_depth id_ownerships in
        own_to_chc (id, pos) id_ownerships (PtrPred(id, pos, make_idx_list depth, fvs)))
        ref_ids in
      let sl_then = make_sl pos_then in
      let sl_else = make_sl pos_else in
      let sl_postif = make_sl pos_postif in
      let sl1 = List.concat (List.map (outer_constrs_iter ("then" :: ifel)) c_lis1) in
      let sl2 = List.concat (List.map (outer_constrs_iter ("else" :: ifel)) c_lis2) in
      sl_then @ sl_else @ sl_postif @
      sl1 @ sl2
    | CHCLetAddPtr (id1, id2, exp, pos) -> 
      let pos' = (string_of_int pos) ^ (ifel_to_str ifel) in
      let sl = exp_to_smtlib exp in
      let id1_ownerships = find_id (id1,pos') num ownerships in
      let id2_ownerships = find_id (id2,pos') num ownerships in
      let depth1 = max_depth id1_ownerships in
      let depth2 = max_depth id2_ownerships in
      let chc1 = own_to_chc (id1, pos') id1_ownerships (PtrPred(id1, pos', make_idx_list depth1, fvs)) in
      let chc2 = own_to_chc (id1, pos') id2_ownerships (PtrPred(id2, pos', make_idx_list depth2, fvs)) in
      if contains_element eq0_list id2 && sl = Id "1" then
        (remove_element eq0_list id2;
        chc1 @ chc2)
      else if contains_element eq0_list id2 then  
          raise (Error "prog_to_chc error")
      else
        chc1 @ chc2
    | CHCAlloc (id, _, _, pos) | CHCAssignInt (id,_,pos) ->
      let pos' = (string_of_int pos) ^ (ifel_to_str ifel) in
      let id_ownerships = find_id (id,pos') num ownerships in
      let depth = max_depth id_ownerships in
      own_to_chc (id, pos') id_ownerships (PtrPred(id, pos', make_idx_list depth, fvs))
    | CHCAliasAddPtr (id1, id2, _, pos) | CHCAlias (id1,id2,pos) ->
      let pos' = (string_of_int pos) ^ (ifel_to_str ifel) in
      let id1_ownerships = find_id (id1,pos') num ownerships in
      let id2_ownerships = find_id (id2,pos') num ownerships in
      let depth1 = max_depth id1_ownerships in
      let depth2 = max_depth id2_ownerships in
      let chc1 = own_to_chc (id1, pos') id1_ownerships (PtrPred(id1, pos', make_idx_list depth1, fvs)) in
      let chc2 = own_to_chc (id1, pos') id2_ownerships (PtrPred(id2, pos', make_idx_list depth2, fvs)) in
      chc1 @ chc2
    | CHCAliasDeref (id1, id2, pos) ->
      remove_element eq0_list id2;
      let pos' = (string_of_int pos) ^ (ifel_to_str ifel) in
      let id1_ownerships = find_id (id1,pos') num ownerships in
      let id2_ownerships = find_id (id2,pos') num ownerships in
      let depth1 = max_depth id1_ownerships in
      let depth2 = max_depth id2_ownerships in
      let chc1 = own_to_chc (id1, pos') id1_ownerships (PtrPred(id1, pos', make_idx_list depth1, fvs)) in
      let chc2 = own_to_chc (id1, pos') id2_ownerships (PtrPred(id2, pos', make_idx_list depth2, fvs)) in
      chc1 @ chc2
    | CHCAssignRef (id1, id2, pos) ->
      eq0_list := id1 :: !eq0_list;
      let pos' = (string_of_int pos) ^ (ifel_to_str ifel) in
      let id1_eq0_ownerships = find_id ((id1^"_eq0"),pos') num ownerships in
      let id1_non0_ownerships = find_id ((id1^"_non0"),pos') num ownerships in
      let id2_ownerships = find_id (id2,pos') num ownerships in
      let depth1 = max_depth id1_eq0_ownerships in
      let depth2 = max_depth id2_ownerships in
      let chc1_eq0 = own_to_chc_eq0 (id1,pos') id1_eq0_ownerships (PtrPred(id1, pos', make_idx_list depth1, fvs)) in
      let chc1_non0 = own_to_chc_non0 (id1,pos') id1_non0_ownerships (PtrPred(id1, pos', make_idx_list depth1, fvs)) in
      let chc2 = [Imply(Id "true", (PtrPred(id2, pos', make_idx_list depth2, fvs)))] in
      chc1_eq0 @ chc1_non0 @ chc2
    | CHCApp (_, args, pos) -> 
      let pos' = (string_of_int pos) ^ (ifel_to_str ifel) in
      List.concat (List.map
      (fun arg ->
        match arg with
        | Var id ->
          let id_ownerships = find_id (id,pos') num ownerships in
          if id_ownerships = [] then [] else
            let depth = max_depth id_ownerships in
            own_to_chc (id, pos') id_ownerships (PtrPred(id, pos', make_idx_list depth, fvs))
        | _ -> raise (Error "prog_to_chc error"))
      args)
    | CHCLetDeref (id1, id2, pos) ->
      eq0_list := id2 :: !eq0_list;
      let pos' = (string_of_int pos) ^ (ifel_to_str ifel) in
      let id1_ownerships = find_id (id1,pos') num ownerships in
      let id2_eq0_ownerships = find_id ((id2^"_eq0"),pos') num ownerships in
      let id2_non0_ownerships = find_id ((id2^"_non0"),pos') num ownerships in
      let depth1 = max_depth id1_ownerships in
      let depth2 = max_depth id2_eq0_ownerships in
      let chc1 = own_to_chc (id1, pos') id1_ownerships (PtrPred(id1, pos', make_idx_list depth1, fvs)) in
      let chc2_eq0 = own_to_chc_eq0 (id2,pos') id2_eq0_ownerships (PtrPred(id2, pos', make_idx_list depth2, fvs)) in
      let chc2_non0 = own_to_chc_non0 (id2,pos') id2_non0_ownerships (PtrPred(id2, pos', make_idx_list depth2, fvs)) in
      chc1 @ chc2_eq0 @ chc2_non0
    | CHCLetInt (_, exp, pos) ->
      (match exp with
      | AppExp (_, exps) ->
        let pos' = (string_of_int pos) ^ (ifel_to_str ifel) in
        List.concat (List.map
        (fun e ->
          match e with
          | Var id ->
            let id_ownerships = find_id (id,pos') num ownerships in
            if id_ownerships = [] then [] else
              let depth = max_depth id_ownerships in
              own_to_chc (id, pos') id_ownerships (PtrPred(id, pos', make_idx_list depth, fvs))
          | _ -> raise (Error "prog_to_chc error"))
        exps)
      | _ -> [])
    | CHCAssert _ | CHCAssume _ -> [] in
    sl_first @ (outer_constrs_iter [] cons)
  

(* let rec own_to_chc (id,i) h_now l_now fvs ownerships =
match ownerships with
| Own (_,id1,i1,_,Float f) :: rest -> 
  (if f = 0. then 
    (* 真　ならば　参照の篩型 *)
    [Imply(Id "true", PtrPred(id1, i1, FV "i", fvs))]
  else
    own_to_chc (id,i) h_now l_now fvs rest)
| CHigh (_,id1,_,_,_,Int i2) :: rest ->
  (if i2 = 0 then
    (* 係数が0の場合は何もしない *)
    own_to_chc (id,i) h_now l_now fvs rest
  else if i2 > 0 then
    (* 係数が0以外の場合はかけるべき変数とかけ合わせ足す *)
    own_to_chc (id,i) (Add(h_now, Mul(FV id1, Id (string_of_int i2)))) l_now fvs rest
  else
    own_to_chc (id,i) (Add(h_now, Mul(FV id1, Id ("(- " ^ string_of_int (-i2) ^ ")")))) l_now fvs rest)
| CLow (_,id1,_,_,_,Int i2) :: rest ->
  (* 以下CHighと同じ *)
  (if i2 = 0 then
    own_to_chc (id,i) h_now l_now fvs rest
  else if i2 > 0 then 
    own_to_chc (id,i) h_now (Add(l_now, Mul(FV id1, Id (string_of_int i2)))) fvs rest
  else 
    own_to_chc (id,i) h_now (Add(l_now, Mul(FV id1, Id ("(- " ^ string_of_int (-i2) ^ ")")))) fvs rest)
| DHigh (_,_,_,_,Int i2) :: rest ->
  (if i2 = 0 then
    own_to_chc (id,i) h_now l_now fvs rest
  else if i2 > 0 then
    own_to_chc (id,i) (Add(h_now, Id (string_of_int i2))) l_now fvs rest
  else
    own_to_chc (id,i) (Add(h_now, Id ("(- " ^ string_of_int (-i2) ^ ")"))) l_now fvs rest)
| DLow (_,_,_,_,Int i2) :: rest ->
  (if i2 = 0 then
    own_to_chc (id,i) h_now l_now fvs rest
  else if i2 > 0 then
    own_to_chc (id,i) h_now (Add(l_now, Id (string_of_int i2))) fvs rest
  else 
    own_to_chc (id,i) h_now (Add(l_now, Id ("(- " ^ string_of_int (-i2) ^ ")"))) fvs rest)
| _ :: rest ->
  own_to_chc (id,i) h_now l_now fvs rest
| [] -> 
  (* 配列のインデックスが所有範囲の上限より大きい　ならば　篩型の述語？
    配列のインデックスが所有範囲の下限より小さい　ならば　篩型の述語？ *)
  [Imply(Gt(Id "i", h_now), PtrPred(id, i, FV "i", fvs));
    Imply(Lt(Id "i", l_now), PtrPred(id, i, FV "i", fvs))] *)

(* let collect_ownchc ownerships num fvs = 
(* 所有権を持つ（参照に束縛される）変数名 *)
  let ids = get_id ownerships num in
  List.concat (List.map (fun (id,i) -> own_to_chc (id,i) fvs (find_id (id,i) num ownerships)) ids) *)

(* 
num 関数の番号
(sl, (el,eh,f))　篩型の述語，(所有範囲の下限，所有範囲の上限，所有権の値) 
z3から得た所有権の解を篩型と結びつける *)
let ownexp_to_ownchc_sub num (sl, (el,eh,f)) = 
  match sl, el with
  (* 所有範囲の下限と上限が定まっていないならば何も返さない *)
  | _, ENull -> []
  | PtrVarPred(num',_,_,idx_list,_), _ when num' = num ->
    if f = 0. then
      (* 所有権の値が0の場合
      真　ならば　篩型の述語？ *)
      [Imply(Id "true", sl)]
    else 
      (* 所有権の値が0でない場合 *)
      let sll = exp_to_smtlib el in
      let slh = exp_to_smtlib eh in
      (* 配列のインデックスが所有範囲の上限より大きい　ならば　篩型の述語？
      配列のインデックスが所有範囲の下限より小さい　ならば　篩型の述語？ *)
      let depth = List.length idx_list in
      let idx = Id (Format.sprintf "i%n" depth) in
      [Imply(Gt(idx, slh), sl);
       Imply(Lt(idx, sll), sl)]
  | _ -> []

(* varpred_count 篩型の述語，(所有範囲の下限，所有範囲の上限，所有権の値)のリスト 
num 関数の番号 *)
let ownexp_to_ownchc varpred_count num =
  List.concat (List.map (ownexp_to_ownchc_sub num) (list_to_set varpred_count []))
