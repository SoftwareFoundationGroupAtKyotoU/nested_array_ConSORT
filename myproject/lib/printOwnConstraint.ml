open Util
open Format

(* 所有権計算に必要なsmtlibでの変数宣言 *)
let rec print_declare oc var_locations fvs fun_num =
  let formatter = formatter_of_out_channel oc in
  (List.iter
    (fun (id,(pos,branch_trace)) ->
      (* 所有権を表す変数の宣言　o_(関数のシリアル番号)_(参照変数名)_(関数内での位置を表す整数)_(then or else) *)
      fprintf formatter "(declare-fun o_%d_%s_%d_%a () Real)\n" fun_num id pos pp_branch_trace branch_trace;
      (* 下限の係数を宣言 *)
       print_declare_c oc fvs "l" id pos branch_trace fun_num;
      (* 所有権を表す下限の一次式の切片の宣言 
       [d + c1*x1 + ..., d' + c1'*x1 + ...] -> o のd
       d_(関数のシリアル番号)_l_(参照変数名)_(関数内での位置を表す整数)_(if or el) *)
       fprintf formatter "(declare-fun d_%d_l_%s_%d_%a () Int)\n" fun_num id pos pp_branch_trace branch_trace;
       (* 上限の係数を宣言 *)
       print_declare_c oc fvs "h" id pos branch_trace fun_num;
      (* 所有権を表す上限の一次式の切片の宣言 
       d_(関数のシリアル番号)_h_(参照変数名)_(関数内での位置を表す整数)_(if or el) *)
       fprintf formatter "(declare-fun d_%d_h_%s_%d_%a () Int)\n" fun_num id pos pp_branch_trace branch_trace;
       ) var_locations);
(* 所有範囲の上限または下限の宣言 *)
and print_declare_c oc fvs l_or_h id pos branch_trace fun_num =
  let formatter = formatter_of_out_channel oc in
  List.iter
    (fun fv ->
      (* smtlibに渡す参照の範囲の上限下限を表す一次式のうち，環境の自由変数の係数定数を宣言
      [d + c1*x1 + ..., d' + c1'*x1 + ...] -> o のc1やc1'
      c_(関数のシリアル番号)_(l(下限) or h(上限))_(自由変数名)_(参照変数名)_(関数内での位置を表す整数)_(if or el) *)
      fprintf formatter "(declare-fun c_%d_%s_%s_%s_%d_%a"
       fun_num l_or_h fv id pos pp_branch_trace branch_trace) fvs

(* smtlib形式で変数宣言を行う
関数評価前と評価後の所有権の範囲を表す
print_declareと大体同じ *)
let rec print_declare_begin_and_end oc varown_count fvs fun_num =
  let formatter = formatter_of_out_channel oc in
  (List.iter
    (fun (id,b_or_e,fun_num') ->
       if fun_num' = fun_num then
        (* 所有権を表す変数の宣言　o_(関数のシリアル番号)_(参照変数名)_(b(評価前) or e(評価後)) *)
        (fprintf formatter "(declare-fun o_%d_%s_%s () Real)\n" fun_num id b_or_e;
         (* 下限の係数を宣言 *)
         print_declare_b_and_e_c oc fvs "l_" id b_or_e fun_num;
         (* 所有権を表す下限の一次式の切片の宣言 *)
         fprintf formatter "(declare-fun d_%d_l_%s_%s () Int)\n" fun_num id b_or_e;
         (* 上限の係数を宣言 *)
         print_declare_b_and_e_c oc fvs "h_" id b_or_e fun_num;
         (* 所有権を表す上限の一次式の切片の宣言 *)
         fprintf formatter "(declare-fun d_%d_h_%s_%s () Int)\n" fun_num id b_or_e)
       else 
         ()
       ) varown_count);
(* 関数評価前，評価後の上限下限の定数係数の宣言 *)
and print_declare_b_and_e_c oc fvs l_or_h id b_or_e fun_num =
  let formatter = formatter_of_out_channel oc in
  List.iter
    (fun fv ->
      fprintf formatter "(declare-fun c_%d_%s_%s_%s_%s" fun_num l_or_h fv id b_or_e;
       ) fvs