# nested_array_ConSORT
## 使い方
- dune buildでビルド
- dune exec myproject ファイルのパス でファイル中のプログラムの所有権推論　結果はout_sat_ans.amt2出力

    ` dune exec myproject ./example/positive_example/init_10.imp `

- dune exec myproject ファイルのパス print_program でファイル中のプログラム全体の構文木を文字列化したものを標準出力

    `  dune exec myproject ./example/positive_example/init_10.imp print_program `

## その他
- example内に検証できるプログラム例をいくつか置いておいます．パス名を変えて遊んでみて下さい．