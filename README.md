# nested_array_ConSORT
## 使い方
- dune buildでビルド
- dune exec myproject ファイルのパス でファイル中のプログラムの所有権推論　結果はout_sat_ans.amt2出力

    ```
    dune exec myproject -- ./example/positive_example/init_10.imp 
    ```

    - busyと出る時
    ```
    dune exec --build-dir="_tmp" myproject -- ./example/positive_example/init_10.imp  
    ```

    - すでに所有権の検査を終えていて篩型(assert)のみの検査を行いたい場合
    ```
    dune exec myproject -- ./example/positive_example/init_10.imp  -refinement 
    ```

    - 関数定義で行列の型アノテーションを全て記載している場合(普通と比較して速く検査が終了することが予想される)
    ```
    dune exec myproject -- ./example/positive_example/init_10.imp  -full_annotated 
    ```
    
    - 所有権型推論でヒューリスティクスを用いない場合
    ```
    dune exec myproject -- ./example/positive_example/init_10.imp  -random_assignment
    ```

     - aliasの自動挿入を行う場合(aliasの自動挿入が正しく行われる保証はなし)
    ```
    dune exec myproject -- ./example/positive_example/init_10.imp  -insert_alias
    ```

- dune exec myproject ファイルのパス print_program でファイル中のプログラム全体の構文木を文字列化したものを標準出力

    ```  
    dune exec myproject -- ./example/positive_example/init_10.imp -print_program 
    ```

- 特定のディレクトリにある全てのimpファイルの検査をしたい時
    ```  
    make run DIR=./example/...
    ```
- 上記のオプションを追加したい時
    ```  
    make run DIR=./example/omit_alias OPTS="-insert_alias"
    ```
- ./example以下の/positive_exampleと/negative_exampleと/much_time_exampleにある全てのimpファイルの検査をしたい時
    ```  
    make run_all
    ```

## その他
- example内に検証できるプログラム例をいくつか置いておいます．パス名を変えて遊んでみて下さい．
- 篩型検査にエルダリカを用いる場合mainのhoice部分を以下で書き換え  
<エルダリカのパス> -hsmt ./experiment/out_chc.smt2 > experiment/chc_result  
例  
~/Downloads/eldarica-2.2/eld -hsmt ./experiment/out_chc.smt2