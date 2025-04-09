(** Module for refinement constraint generation *)

open Syntax
open CHCcollectConstraint
open Util
open GenerateSmtlibConstraint
open CHCSyntax
open SmtlibSyntax

(* 参照型の(変数id, (プログラムの位置l, ifel))のリスト
  ifelはif文の分岐情報を表す *)
let id_count_chc = ref []

(* 篩型の述語，(所有範囲の下限，所有範囲の上限，所有権の値)のリスト *)
let varpred_count = ref []

(* 指定されたidとifelに対応するプログラムの位置lを環境envから検索 *)
let rec lookup_ifel id ifel env =
  match env with
  | [] -> raise Unbound
  | (x, (l, lst)) :: nenv -> if id = x && ifel = lst then l else lookup_ifel id ifel nenv

(* id_count_chcに新しい変数idを追加または既存のidの情報を更新 *)
let new_id id l ifel =
  try
    let i = lookup_ifel id ifel !id_count_chc in
    if i = l then ()
    else id_count_chc := (id, (l, ifel)) :: !id_count_chc
  with 
    Unbound -> id_count_chc := (id, (l, ifel)) :: !id_count_chc
(* 
(* id_count_chcの表示，使われてなさそう *)
let rec print_id_count_chc id_count_chc = 
  match id_count_chc with
  | [] -> print_newline ()
  | (id, (l, ifel)) :: id_count_chc' -> print_string ("(" ^ id ^ ", "); print_int l; print_string (ifel_to_str ifel); print_string ") "; print_id_count_chc id_count_chc' *)

(* 指定されたidとifelに対応するプログラムの直前の位置lを返す *)
let rec lookup_pre_ifel id ifel env = 
  match env with
  | [] -> raise Unbound
  | (x, _) :: nenv -> if id = x then lookup_ifel id ifel nenv else lookup_pre_ifel id ifel nenv

(* 変数名，添え字を表す変数，自由変数の集合，ifの分岐を表す文字列を受け取って
  変数名，プログラムの位置とifの分岐を表す文字列，添え字を表す変数，自由変数の集合
*)
let ptrpred id i_sl n_sl ifel =
  PtrPred(id, (string_of_int (lookup_ifel id ifel !id_count_chc)) ^ (ifel_to_str ifel), i_sl, n_sl)

(* 変数名，何か，何か，ifの分岐を表す文字列を受け取って
  変数名，直前のプログラムの位置とifの分岐を表す文字列，何か，何かの組を返す 
    何かの一つ目はおそらく篩型の述語*)
let ptrpred_p id i_sl n_sl ifel =
  PtrPred(id, (string_of_int (lookup_pre_ifel id ifel !id_count_chc)) ^ (ifel_to_str ifel), i_sl, n_sl)

(* #付きの変数名の抽出 *)
let find_subst ftid_ft e = 
  match ftid_ft with
  | (RawId _, FTInt _) -> []
  | (HashId id, FTInt _) -> [(id, e)]
  | (RawId _, FTRef _) -> []
  | _ -> raise ConstrError

(* string_of_intの改造版
負の数の場合は括弧をつける *)
let my_string_of_int i = 
  if Z.leq i Z.zero then Z.to_string i 
  else Format.sprintf "(%s)" (Z.to_string i)

(* 指定のifelと同じ分岐の変数一覧のリストを返す *)
let rec find_id_count ifel id_count res = 
  match id_count with
  | [] -> res
  | (x, (_,lst)) :: id_count' -> 
    if List.mem x res || ifel <> lst then 
      find_id_count ifel id_count' res 
    else 
      find_id_count ifel id_count' (x :: res)

(* 重複の内容にリストの結合をする *)
let rec union_list ls1 ls2 = 
  match ls1 with
  | [] -> ls2
  | x :: ls1' -> if List.mem x ls2 then union_list ls1' ls2 else union_list ls1' (x :: ls2)

(** Main procedure for generating the refinement constraints *)
let rec emit_chc fvs fun_num ifel c =
  match c with
  | CHCIf (e,cs1,cs2,pos) -> 
    (* if評価前の変数名リスト *)
    let ids_pre = find_id_count ifel !id_count_chc [] in 
    (* ifの分岐を考慮しながらid_count_chcの更新 *)
    List.iter (fun id -> new_id id pos ("if" :: ifel); new_id id pos ("el" :: ifel)) ids_pre;
    (* if節前ならばthen節，if節前ならばelse節という所有権状態の制約？ *)
    let ss_pre = 
      List.concat (List.map 
        (fun id -> 
          [Imply(ptrpred id (FV "i") fvs ifel, ptrpred id (FV "i") fvs ("if" :: ifel));
           Imply(ptrpred id (FV "i") fvs ifel, ptrpred id (FV "i") fvs ("el" :: ifel))]
        ) ids_pre) in
    (* then節側の制約をsmtlibが読める制約の形に直す *)
    let ss1 = List.concat (List.map (emit_chc fvs fun_num ("if" :: ifel)) cs1) in
    (* 条件式が成り立つならばthen節の制約が成り立つ，という形に変更 *)
    let ss1' = List.map (fun s -> Imply(exp_to_smtlib e, s)) ss1 in
    (* else節側の制約をsmtlibが読める制約の形に直す *)
    let ss2 = List.concat (List.map (emit_chc fvs fun_num ("el" :: ifel)) cs2) in
    (* 条件式が成り立たないならばelse節の制約が成り立つ，という形に変更 *)
    let ss2' = List.map (fun s -> Imply(Not(exp_to_smtlib e), s)) ss2 in
    (* then評価後の参照変数リスト *)
    let ids_post_if = find_id_count ("if" :: ifel) !id_count_chc [] in
    (* else評価後の参照変数リスト *)
    let ids_post_el = find_id_count ("el" :: ifel) !id_count_chc [] in
    (* then節，else節評価後の変数名のリストを結合 *)
    let ids_post = union_list ids_post_if ids_post_el in
    (* 変数名のリストをもとにif式評価後のid_count_chcの更新（プログラムを表す位置の変化を反映） *)
    List.iter (fun id -> new_id id (pos+1) ifel) ids_post; 
    (* 条件節が成り立つならば(then節ならば，if節前)という制約リスト？ *)
    let ss_post_if = 
      List.map 
        (fun s -> Imply(exp_to_smtlib e, s))
        (List.map 
          (fun id -> 
            Imply(ptrpred id (FV "i") fvs ("if" :: ifel), ptrpred id (FV "i") fvs ifel)
          ) ids_post_if) in
    (* 条件節が成り立たないならば(else節ならば，if節前)という制約リスト？ *)
    let ss_post_el = 
      List.map 
        (fun s -> Imply(Not(exp_to_smtlib e), s))
        (List.map 
          (fun id -> 
            Imply(ptrpred id (FV "i") fvs ("el" :: ifel), ptrpred id (FV "i") fvs ifel)
          ) ids_post_el) in
    (* 制約の結合 *)
    ss_pre @ ss1' @ ss2' @ ss_post_if @ ss_post_el
  | CHCLetInt (id,e,pos) -> 
    (match e with
     | Deref id' -> (* let id = *id' in ... *)
     (* xの篩型を追加？ *)
       intpred_env := (id, []) :: !intpred_env;
       (* id'の0番目の篩型ならばidの篩型という制約 *)
       [Imply(ptrpred id' (Id "0") fvs ifel, IntPred(id, ["v"]))]
     | AppExp (id',es) -> (* let x = f y1 y2 ... in ... *)
       (* 関数の評価前の型，評価後の型，返り値の型 *)
       let (ftid_fts1, ftid_fts2, ft_r) = lookup id' !fn_env_chc in
       (* #付きの仮引数と対応する実引数の組 *)
       let subst = List.concat (List.map2 find_subst ftid_fts1 es) in
       (* #付きの実引数名を抽出する関数 *)
       let rec find_hasharg ftid_fts es =
         match ftid_fts, es with
         | (HashId _, FTInt _) :: ftid_fts', e' :: es' -> 
          (match exp_to_smtlib e' with
          | FV id_depended -> id_depended :: find_hasharg ftid_fts' es'
          | _ -> raise (Error "find_hasharg error"))
         | _ :: ftid_fts', _ :: es' -> find_hasharg ftid_fts' es'
         | [], _ -> []
         | _, _ -> raise (Error "find_hasharg error")
       in
       (* #付きの実引数名のリスト *)
       let ids_depended = find_hasharg ftid_fts1 es in
       let before_app ftid_ft e = 
         match ftid_ft with
         (* 参照型の引数 *)
         | (RawId id_arg, FTRef ((FTInt VarPred),el,eh,f)) -> (* #ありの整数型引数 *)
          (* 実引数名 *)
          (match exp_to_smtlib e with
          | FV fv ->
            (* 関数の順番 *)
            let num = lookup id' fun_num in
            (* 参照型の引数に関する述語を追加 *)
            varpred_count := (PtrVarPred(num, id_arg, "b", (FV "i"), fvs), (el,eh,f)) :: !varpred_count;
            (* 
            　　　実引数の参照型の述語
            　　かつ
                  自由変数の述語
            ならば
            　　仮引数の参照型の述語
            　　*)
            [Imply(Ands((ptrpred fv (FV "i") fvs ifel) :: List.map (fun id -> IntPred(id, id::(lookup id !intpred_env))) ids_depended),
               PtrVarPred(num, id_arg, "b", (FV "i"), ids_depended))]
           | _ -> raise (Error "CHC AppExp error"))
        | (RawId id_arg, FTInt VarPred) -> (* #なしの整数型引数 *)
          (match exp_to_smtlib e with 
          (* 実引数名 *)
          | FV id_e -> 
            (* 篩型で依存できる変数 *)
            let fvs_int = lookup id_e !intpred_env in 
            (* 仮引数の篩型を追加 *)
            intpred_env := (id_arg, []) :: !intpred_env; 
            (* 関数の順番 *)
            let num = lookup id' fun_num in
            (* 実引数の述語ならば仮引数の述語 *)
            [Imply(IntPred(id_e, "v" :: fvs_int), IntVarPred(num, id_arg, ["v"]))] 
          | _ -> raise (Error "CHC AppExp error"))
        | (HashId id_arg, FTInt VarPred) -> 
          (match exp_to_smtlib e with 
          (* 実引数名 *)
          | FV id_e -> 
            (* 篩型で依存できる変数 *)
            let fvs_int = lookup id_e !intpred_env in 
            (* 仮引数の篩型を追加 *)
            intpred_env := (id_arg, []) :: !intpred_env;
            (* 関数の順番 *) 
            let num = lookup id' fun_num in
            (* 実引数の述語ならば仮引数の述語 *)
            [Imply(IntPred(id_e, "v" :: fvs_int), IntVarPred(num, id_arg, ["v"]))] 
          | _ -> raise (Error "CHC AppExp error"))
         | (RawId _, FTRef ((FTInt sl),_,_,_)) -> (* 篩型指定ありの参照型 *)
          (match exp_to_smtlib e with 
            (* 実引数名 *)
            | FV id_e -> 
            (* 仮引数の篩型の変数部分を実引数で置き換える *)
            let n_sl = smtlib_subst subst sl in
            (* 
            　　　　実引数の述語
            　　かつ
            　　　　自由変数の述語
            ならば
              　仮引数の指定の篩型の述語
            *)
            [Imply(Ands((ptrpred id_e (FV "i") fvs ifel) :: List.map (fun id -> IntPred(id, id::(lookup id !intpred_env))) ids_depended), n_sl)] 
            | _ -> raise (Error "CHC AppExp error"))
         | (RawId _, FTInt sl) | (HashId _, FTInt sl) -> (* #なしの篩型指定あり整数型引数 *)
          (match exp_to_smtlib e with 
          (* 実引数名 *)
          | FV id_e -> 
            (* 篩型で依存できる変数 *)
            let fvs_int = lookup id_e !intpred_env in 
            (* 実引数の述語ならば篩型指定の述語 *)
            [Imply(IntPred(id_e, "v" :: fvs_int), sl)]
          | _ -> raise (Error "CHC AppExp error"))
         | _ -> raise (Error "CHC AppExp error")
       in
       (* 評価前引数が満たすべき制約に関するリスト *)
       let ss1 = List.concat (List.map2 before_app ftid_fts1 es) in
       let after_app ftid_ft e = 
         match ftid_ft with
         | (RawId id_arg, FTRef ((FTInt VarPred),el,eh,f)) -> (* 篩型指定なしの参照 *)
          (match exp_to_smtlib e with 
            (* 実引数名 *)
          | FV id_e -> 
          (* 関数評価後のid_count_chcを追加 *)
            new_id id_e pos ifel;
              (* 関数の順番 *) 
            let num = lookup id' fun_num in
            (* 仮引数の述語を追加？ *)
            varpred_count := (PtrVarPred(num, id_arg, "e", (FV "i"), fvs), (el,eh,f)) :: !varpred_count;
            (* 
            　　　　仮引数の述語
            　　かつ
            　　　　自由変数の述語
            ならば
            　　実引数の述語
            *)
            [Imply(Ands(PtrVarPred(num, id_arg, "e", (FV "i"), ids_depended) :: List.map (fun id -> IntPred(id, id::(lookup id !intpred_env))) ids_depended), ptrpred id_e (FV "i") fvs ifel)] 
          | _ -> raise (Error "CHC AppExp error"))
         | (RawId _, FTRef ((FTInt sl),_,_,_)) -> (* 篩型指定ありの参照 *)
          (match exp_to_smtlib e with 
          (* 実引数名 *)
          | FV id_e -> 
           let n_sl = smtlib_subst subst sl in
           new_id id_e pos ifel;
            (* 
           　　　　仮引数の指定の篩型の述語
           　　かつ
           　　　　自由変数の述語
           ならば
           　　実引数の述語
           *)
           [Imply(Ands(n_sl :: List.map (fun id -> IntPred(id, id::(lookup id !intpred_env))) ids_depended), ptrpred id_e (FV "i") fvs ifel)] 
          | _ -> raise (Error "CHC AppExp error"))
         | (RawId _, FTInt _) | (HashId _, FTInt _) -> 
           []
         | _ -> raise ConstrError
       in
       (* 評価後引数が満たすべき制約に関するリスト *)
       let ss2 = List.concat (List.map2 after_app ftid_fts2 es) in
       (* #付きの仮引数に対応する実引数の抽出 *)
       let find_fv' ftid_ft e = 
         match ftid_ft, e with
         | (HashId _, _), Var x -> [x]
         | _ -> []
       in
       (* #付きの仮引数に対応する実引数名のリスト *)
       let fvs' = List.concat (List.map2 find_fv' ftid_fts1 es) in
       let sl_ret = 
         match ft_r with (* 返り値の型で分類 *)
         | FTInt VarPred -> (* 篩型指定なし整数 *)
         (* 返り値を受け取る変数の篩型を追加？ *)
           intpred_env := (id, []) :: !intpred_env;
           let num = lookup id' fun_num in
           (* 
           　　　　関数の返り値の篩型の述語
           　　かつ
           　　　　#付きの仮引数の篩型の述語
           ならば
           　　返り値を受け取る変数の述語
           *)
           Imply(Ands(IntVarPred(num, "ret", id :: fvs') :: (List.map (fun fv -> IntPred(fv, fv :: (lookup fv !intpred_env))) fvs')), IntPred(id, [id])) 
         | FTInt sl -> (* 篩型指定あり整数 *)
         (* 返り値を受け取る変数の篩型を追加？ *)
           intpred_env := (id, []) :: !intpred_env; 
        (* 関数の返り値の篩型の述語ならば返り値を受け取る変数の述語 *)
           Imply(sl, IntPred(id, [id])) 
         | _ -> raise ConstrError
       in
       (* 制約の結合 *)
       sl_ret :: ss1 @ ss2
     | ConstRandInt ->(* 不定値整数の場合　let x = _ in *)
       intpred_env := (id, []) :: !intpred_env;
       (* 真　ならば　新たに定義された変数の述語
       どんな述語も許容？ *)
       [Imply((Id "true"), IntPred(id, [id]))]
     | e -> (* そのほかの場合　let x = e in　*)
     (* e中の自由変数 *)
       let fvs' = fvs_of_exp e in
       (* 自由変数をハッシュ付き，ハッシュなしにわけて返す *)
       let rec find_hash fvs' h r = 
         (match fvs' with
          | fv :: rest -> 
            if List.mem fv fvs then find_hash rest (fv::h) r else find_hash rest h (fv::r) 
          | [] -> (h, r)) 
       in
       (* ハッシュ付きの変数，ハッシュなしの整数変数のリストの組 *)
       let (hash_fvs', fvs') = find_hash fvs' [] [] in
       (* 篩型で依存できる環境の更新？ *)
       intpred_env := (id, hash_fvs') :: !intpred_env;
       (* 
              新たに束縛される変数xと束縛のための式の値eが等しい
          かつ
              ハッシュなしの整数変数の篩型の述語
       ならば
          新たに束縛される変数xの篩型の述語
          *)
       [Imply(Ands(Eq(Id("v"), exp_to_smtlib e) :: (List.map (fun fv -> IntPred(fv, fv :: (lookup fv !intpred_env))) fvs')), IntPred(id, "v" :: hash_fvs'))])
  | CHCLet (id1,id2,l) -> (*　let x = y(参照) in ... *)
  (* 代入評価後のid_count_chcを追加 *)
    new_id id1 l ifel; new_id id2 l ifel;
      (* 
    代入前のyの篩型の述語　ならば　代入後のyの篩型の述語
    代入前のyの篩型の述語　ならば　代入後のxの篩型の述語 *)
    [Imply(ptrpred_p id2 (FV "i") fvs ifel, ptrpred id2 (FV "i") fvs ifel);
     Imply(ptrpred_p id2 (FV "i") fvs ifel, ptrpred id1 (FV "i") fvs ifel)]
  | CHCLetAddPtr (id1,id2,e,l) -> (*　let x = y(参照) + z in ... *)
    (* 代入評価後のid_count_chcを追加 *)
    new_id id1 l ifel; new_id id2 l ifel;
    (match e with
     | ILit i -> (*　let x = y(参照) + i(整数) in ... *)
     (* 
    代入前のyの篩型の述語　ならば　代入後のyの篩型の述語;
    代入前のyの篩型の述語　ならば　代入後のxの篩型の述語{v | phi}のphi中のvをv-nにしたもの *)
       [Imply(ptrpred_p id2 (FV "i") fvs ifel, ptrpred id2 (FV "i") fvs ifel);
        Imply(ptrpred_p id2 (FV "i") fvs ifel, ptrpred id1 (Sub((FV "i"), (Id (my_string_of_int i)))) fvs ifel)]
     | Var x -> (*　let id1 = id2(参照) + x(変数) in ... *)
       (* 
    代入前のyの篩型の述語　ならば　代入後のyの篩型の述語;
        zの篩型の述語
    ならば
        (代入前のyの篩型の述語　ならば　代入後のxの篩型の述語{v | phi}のphi中のvをv-zにしたもの) *)
       [Imply(ptrpred_p id2 (FV "i") fvs ifel, ptrpred id2 (FV "i") fvs ifel);
        Imply(IntPred(x, x::(lookup x !intpred_env)), Imply(ptrpred_p id2 (FV "i") fvs ifel, ptrpred id1 (Sub((FV "i"), (FV x))) fvs ifel))] 
     | _ -> raise (Error "CHCLetAddPtr error")
    )
  | CHCAlloc (id,_,l) -> (*　let id = alloc e in ... *)
    new_id id l ifel;
      (* 真　ならば　新たに定義された参照の述語
       どんな述語も許容？ *)
    [Imply(Id "true", ptrpred id (FV "i") fvs ifel)]
  | CHCAssignInt (id,e,l) -> (*　let y := x; ... *)
    new_id id l ifel;
    (match e with
     | Deref id' -> (*　let id := *id'; ... *)
     (* 
          (添え字が0と等しい ならば 評価後のid'の篩型の述語)
        かつ
          (添え字が0と等しくない ならば 評価前のidの篩型)
     ならば
        評価後のidの篩型
      *)
       [Imply(And(Imply(Eq((FV "i"), (Id "0")), ptrpred id' (Id "0") fvs ifel), 
                  Imply(Not(Eq((FV "i"), (Id "0"))), ptrpred_p id (FV "i") fvs ifel)),
              ptrpred id (FV "i") fvs ifel)]
     | Var x -> (*　let y := x(整数); ... *)
       (* xの篩型が依存できる変数のリスト？ *)
       let vars = lookup x !intpred_env in
       (* 
            (添え字が0と等しい ならば xの篩型の述語)
          かつ
            (添え字が0と等しくない ならば 評価前のyの篩型の述語)
       ならば
          評価後のyの篩型
       *)
       [Imply(And(Imply(Eq((FV "i"), (Id "0")), IntPred(x, "v" :: vars)), 
                  Imply(Not(Eq((FV "i"), (Id "0"))), ptrpred_p id (FV "i") fvs ifel)),
              ptrpred id (FV "i") fvs ifel)]
     | e -> (*　let y := e; ... *)
        (* (添え字が0と等しい ならば 評価後のeの篩型の述語)
        かつ
          (添え字が0と等しくない ならば 評価前のyの篩型の述語)
      ならば
        評価後のyの篩型の述語 *)
       [Imply(And(Imply(Eq((FV "i"), (Id "0")), Eq(Id("v"), exp_to_smtlib e)), 
                  Imply(Not(Eq((FV "i"), (Id "0"))), ptrpred_p id (FV "i") fvs ifel)),
              ptrpred id (FV "i") fvs ifel)])
  | CHCAlias (id1,id2,pos) -> (*　alias(x = y); ... *)
    new_id id1 pos ifel; new_id id2 pos ifel;
      (* (評価前のyの篩型の述語　かつ　評価前のxの篩型の述語) ならば 評価後のyの篩型の述語
        (評価前のyの篩型の述語　かつ　評価前のxの篩型の述語) ならば 評価後のxの篩型の述語 *)
    [Imply(And(ptrpred_p id2 (FV "i") fvs ifel, ptrpred_p id1 (FV "i") fvs ifel), ptrpred id2 (FV "i") fvs ifel);
     Imply(And(ptrpred_p id2 (FV "i") fvs ifel, ptrpred_p id1 (FV "i") fvs ifel), ptrpred id1 (FV "i") fvs ifel)]
  | CHCAliasAddPtr (id1,id2,e,l) -> (*　alias(x = y + e); ... *)
    new_id id1 l ifel; new_id id2 l ifel;
    (match e with
     | ILit i -> (*　alias(x = y + i(整数)); ... *)
     (* (評価前のyの篩型の述語　かつ　評価前のxの篩型の述語のvをv-iにしたもの) ならば 評価後のyの篩型の述語
        (評価前のyの篩型の述語　かつ　評価前のxの篩型の述語vをv-iにしたもの) ならば 評価後のxの篩型の述語vをv-iにしたもの *)
       [Imply(And(ptrpred_p id2 (FV "i") fvs ifel, ptrpred_p id1 (Sub((FV "i"), (Id (my_string_of_int i)))) fvs ifel), ptrpred id2 (FV "i") fvs ifel);
        Imply(And(ptrpred_p id2 (FV "i") fvs ifel, ptrpred_p id1 (Sub((FV "i"), (Id (my_string_of_int i)))) fvs ifel), ptrpred id1 (Sub((FV "i"), (Id (my_string_of_int i)))) fvs ifel)]   
     | Var x -> (*　alias(id1 = id2 + x(変数)); ... *)
       (* 
          zの篩型の述語
       ならば
          (評価前のyの篩型の述語　かつ　評価前のxの篩型の述語のvをv-iにしたもの) ならば 評価後のyの篩型の述語;
          zの篩型の述語
       ならば
          (評価前のyの篩型の述語　かつ　評価前のxの篩型の述語vをv-iにしたもの) ならば 評価後のxの篩型の述語vをv-iにしたもの *)
       [Imply(IntPred(x, x::(lookup x !intpred_env)), Imply(And(ptrpred_p id2 (FV "i") fvs ifel, ptrpred_p id1 (Sub((FV "i"), (FV x))) fvs ifel), ptrpred id2 (FV "i") fvs ifel));
        Imply(IntPred(x, x::(lookup x !intpred_env)), Imply(And(And(ptrpred_p id2 (FV "i") fvs ifel, ptrpred_p id1 (Sub((FV "i"), (FV x))) fvs ifel), IntPred(x, x::(lookup x !intpred_env))), ptrpred id1 (Sub((FV "i"), (FV x))) fvs ifel))] 
     | _ -> raise (Error "CHCAliasAddPtr error")
    )
  | CHCAssert (e,_) -> (*　assert( e ); ... *)
    (* e中の自由変数 *)
    let fvs = fvs_of_exp e in
    if fvs = [] then
      (* assert中の論理式eに自由変数が含まれていないならばeをそのまま制約に *)
      [exp_to_smtlib e]
    else
      (* 
      自由変数の篩型の述語　ならば　assert中の論理式e *)
      [Imply(Ands(List.map (fun fv -> let vars = lookup fv !intpred_env in IntPred(fv, fv :: vars)) fvs), exp_to_smtlib e)]
  | _ -> raise ConstrError(*追加分，あとで消す*)

(* #付きの変数名を抜き出す *)
let find_fv ftid_ft = 
  match ftid_ft with
  | (HashId id, _) -> [id]
  | _ -> []

(* #なしの変数名を抜き出す *)
let find_ref_id ftid_ft = 
  match ftid_ft with
  | (RawId id, FTRef _) -> [id]
  | _ -> [] 

(* #あるなしに関わらずの変数名を抜き出す *)
let find_id ftid_ft = 
  match ftid_ft with
  | (RawId id, FTRef _) -> [id]
  | (RawId id, FTInt _) -> [id]
  | (HashId id, FTInt _) -> [id]
  | _ -> [] 

(* 関数の引数型のうちidを抽出 *)
let rec assoc_ft id ftid_fts = 
  match ftid_fts with
  (* #なし引数　かつ　参照型の場合は篩型，参照型，所有範囲の下限，所有範囲の上限，所有権の値の組 *)
  | (RawId id_ft, FTRef ((FTInt sl),el,eh,f)) :: _ when id_ft = id -> (sl, FTRef ((FTInt sl),el,eh,f))
  (* #なし引数　かつ　整数型の場合は篩型と整数型の組 *)
  | (RawId id_ft, FTInt sl) :: _ when id_ft = id -> (sl, FTInt sl)
  (* #あり引数　かつ　整数型の場合は篩型と整数型の組 *)
  | (HashId id_ft, FTInt sl) :: _ when id_ft = id -> (sl, FTInt sl)
  (* 他は飛ばす *)
  | _ :: ftid_fts' -> assoc_ft id ftid_fts'
  | [] -> raise Not_found

let ics_to_smtlib ics fun_num =
  (* 関数名，CHCの制約を表すデータ型の値，最後に評価されうる式 *)
  let (id', cs, e_rets) = ics in
  (* 評価前の型，評価後の型，返り値の型 *)
  let (ftid_fts1, ftid_fts2, ft_r) = lookup id' !fn_env_chc in
  (* 自由整数変数(#付きの整数引数)の抽出 *)
  let fvs = List.concat (List.map find_fv ftid_fts1) in
  (* #がつかない　かつ　参照型である引数の抽出 *)
  let ref_ids =  List.concat (List.map find_ref_id ftid_fts1) in
  (* 引数名の抽出 *)
  let ids =  List.concat (List.map find_id ftid_fts1) in
  id_count_chc := List.map (fun id -> (id, (1,[]))) ref_ids;
  intpred_env := [];
  varpred_count := [];
  let g1 id = 
    (* 篩型と引数型の組を抜き出す *)
    let (sl,ft) = assoc_ft id ftid_fts1 in 
    match sl, ft with
    (* 篩型のない参照の場合 *)
    | VarPred, FTRef (_,el,eh,f) -> 
      (* 関数の順番 *)
      let num = lookup id' fun_num in
      varpred_count := (PtrVarPred(num, id, "b", (FV "i"), fvs), (el,eh,f)) :: !varpred_count;
      (* 関数評価前の篩型の述語　ならば　関数評価はじめの篩型の述語 *)
      [Imply(PtrVarPred(num, id, "b", (FV "i"), fvs), ptrpred id (FV "i") fvs [])] 
    (* 篩型のない整数の場合 *)
    | VarPred, FTInt _ -> 
      (* 変数名を追加 *)
      intpred_env := (id, []) :: !intpred_env;
      []
    (* 篩型のある参照の場合 *)
    | _, FTRef _ -> 
      (* 指定の篩型の述語　ならば　関数評価はじめの篩型の述語 *)
      [Imply(sl, ptrpred id (FV "i") fvs [])] 
    (* 篩型のある整数の場合 *)
    | _, FTInt _ -> 
      (* 引数の篩型が依存できる変数のリスト？ *)
      intpred_env := (id, []) :: !intpred_env;
      (* 指定の篩型の述語　ならば　関数評価はじめの篩型の述語 *)
      [Imply(sl, IntPred(id, ["v"]))]
  in
  (* 引数変数の篩型に関する制約 *)
  let s1 = List.concat (List.map g1 ids) in
  (* 関数本体部分の篩型に関する制約 *)
  let ss = List.concat (List.map (emit_chc fvs fun_num []) cs) in
  let g2 id = 
    (* 篩型と関数評価後の引数型の組を抜き出す *)
    let (sl,ft) = assoc_ft id ftid_fts2 in 
    match sl, ft with
    (* 篩型のない参照の場合 *)
    | VarPred, FTRef (_,el,eh,f) -> 
      let num = lookup id' fun_num in
      varpred_count := (PtrVarPred(num, id, "e", (FV "i"), fvs), (el,eh,f)) :: !varpred_count;
      (* 関数評価終わりの篩型の述語　ならば　関数評価後の篩型の述語*)
      [Imply(ptrpred id (FV "i") fvs [], PtrVarPred(num, id, "e", (FV "i"), fvs))] 
    (* 篩型のない整数の場合 *)
    | VarPred, FTInt _ -> 
      []
    (* 篩型のある参照の場合 *)
    | _, FTRef _ -> 
      (* 関数評価終わりの篩型の述語　ならば 指定の篩型の述語　*)
      [Imply(ptrpred id (FV "i") fvs [], sl)] 
    (* 篩型のある整数の場合 *)
    | _, FTInt _ -> 
      (* 関数評価終わりの篩型の述語　ならば 指定の篩型の述語　*)
      [Imply(IntPred(id, ["v"]), sl)]
  in
  (* 関数評価後の引数変数に関する篩型の制約 *)
  let s2 = List.concat (List.map g2 ids) in
  (* 
  ft_r: 返り値型
  cond: 条件節の分岐を表したリスト
  e_ret: 条件節の分岐を辿った際に評価される式 *)
  let g3 ft_r (cond, e_ret) = 
    try 
      let cond_sl = Ands(cond) in
      let e_sl = exp_to_smtlib e_ret in
      (match ft_r, e_sl with
      | FTInt VarPred, FV id_e -> (*　指定の返り値が整数で篩型もなく，実際評価する式が整数変数の場合*)
        let num = lookup id' fun_num in
        let fvs_int = lookup id_e !intpred_env in 
        varpred_count := (IntVarPred(num, "ret", fvs), (Unit, Unit, 0.)) :: !varpred_count;
        (* 条件節の条件を全て満たす　ならば　(実際評価する式の篩型　ならば　返り値の変数の型) *)
        [Imply(cond_sl, Imply(IntPred(id_e, id_e :: fvs_int), IntVarPred(num, "ret", id_e :: fvs)))]
      | FTInt VarPred, Id i -> (*　指定の返り値が整数で篩型もなく，実際評価する式が定数の場合*)
        let num = lookup id' fun_num in
        varpred_count := (IntVarPred(num, "ret", fvs), (Unit, Unit, 0.)) :: !varpred_count;
        (* 条件節の条件を全て満たす　ならば　(返り値の篩型の述語は返り値の定数iを示す) *)
        [Imply(cond_sl, Imply(Eq(FV "v", Id i), IntVarPred(num, "ret", "v" :: fvs)))] 
      | FTInt sl, FV id_e -> (*　指定の返り値が整数で篩型があり，実際評価する式が変数の場合*)
        let fvs_int = lookup id_e !intpred_env in 
        (* 条件節の条件を全て満たす　ならば　(実際評価する式の篩型　ならば　指定の返り値の篩型)  *)
        [Imply(cond_sl, Imply(IntPred(id_e, id_e :: fvs_int), sl))]
      | FTInt sl, Id i -> (*　指定の返り値が整数で篩型があり，実際評価する式が定数の場合*)
        (* 条件節の条件を全て満たす　ならば　(最後に評価される参照の篩型　ならば　指定の返り値の篩型)  *)
        [Imply(cond_sl, Imply(Eq(FV "v", Id i), sl))] 
      | _ -> raise ConstrError)
    with ElimError -> []
  in
  (* 返り値に関する篩型の制約 *)
  let s3 = List.concat (List.map (g3 ft_r) e_rets) in
  (* 制約を連結 *)
  (s1 @ ss @ s2 @ s3, !id_count_chc, !varpred_count, fvs)
  
  (* 関数名と関数の順番の組を返す *)
let rec fun_number all_cs cnt res =
  match all_cs with
  | [] -> res
  | ics :: all_cs' -> 
    let (id,_,_) = ics in
    fun_number all_cs' (cnt+1) ((id, cnt) :: res)

let all_cs_to_smtlib_chc all_cs n =
  (* 関数名と関数の順番の組 *)
  let fun_num = fun_number all_cs 0 [] in
  (* 
  ss: 篩型の制約
  id_count_chc: (変数id, (プログラムの位置l, ifel))のリスト
  varpred_count: 篩型の述語，(所有範囲の下限，所有範囲の上限，所有権の値)のリスト
  fvs: (* 自由整数変数(#付きの整数引数)の抽出 *)*)
  let (ss, id_count_chc, varpred_count, fvs) = ics_to_smtlib (List.nth all_cs n) fun_num in
  (id_count_chc, varpred_count, fvs, ss)
