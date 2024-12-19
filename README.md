# nested_array_ConSORT
## 使い方
- dune buildでビルド
- dune exec myproject ファイルのパス でファイル中のプログラムの所有権推論　結果はout_sat_ans.amt2出力

    ` dune exec myproject ./example/init_10.imp `

- dune exec myproject ファイルのパス 整数 でファイル中のプログラムの推論の第一段階の制約/experiment/out_int.smt2に出力，制約の粒度は整数によって決まる

    `  dune exec myproject ./example/init_10.imp 5 ` 

- dune exec myproject ファイルのパス print_program でファイル中のプログラム全体の構文木を文字列化したものを標準出力

    `  dune exec myproject ./example/init_10.imp print_program `

## その他
- example内に検証できるプログラム例をいくつか置いておいます．パス名を変えて遊んでみて下さい．