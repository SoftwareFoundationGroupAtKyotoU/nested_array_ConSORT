(** An intermediate module to pass the ownership information obtained to the refinment inference phase *)

open Z3Syntax2
open Syntax
open SmtlibSyntax
open Util
open PrintOwnConstraint

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
let rec own_to_chc (id,i) fvs full_ownerships =
  let max_dep = max_depth full_ownerships in
  if is_0own (id,i) full_ownerships 
  then [Imply(Id "true", PtrPred(id, i, make_idx_list max_dep, fvs))] 
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
            [Imply(Gt(Id "i1", h_now), PtrPred(id, i, make_idx_list max_dep, fvs));
            Imply(Lt(Id "i1", l_now), PtrPred(id, i, make_idx_list max_dep, fvs))]
          else
            let sl1 =
              [Imply(Gt(Id (Format.sprintf "i%n" now_depth), h_now), PtrPred(id, i, make_idx_list max_dep, fvs));
              Imply(Lt(Id (Format.sprintf "i%n" now_depth), l_now), PtrPred(id, i, make_idx_list max_dep, fvs))] in
            let sl2 = own_to_chc_sub (Id "0") (Id "0") full_ownerships (now_depth-1) in
            sl1 @ sl2 in
    own_to_chc_sub (Id "0") (Id "0") full_ownerships max_dep

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

let collect_ownchc ownerships num fvs = 
(* 所有権を持つ（参照に束縛される）変数名 *)
  let ids = get_id ownerships num in
  List.concat (List.map (fun (id,i) -> own_to_chc (id,i) fvs (find_id (id,i) num ownerships)) ids)

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
