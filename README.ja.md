# nested_array_ConSORT

[ConSORT](https://www.fos.kuis.kyoto-u.ac.jp/projects/consort/, https://github.com/mamizu-git/Extended_ConSORT) をネストされた配列に対応するよう拡張したツールです。

[English README](README.md)

## 使い方

以下のコマンドはすべて `myproject/` ディレクトリ下で実行してください。

- `.imp` ファイルに対して所有権推論を実行（結果は `out_sat_ans.smt2` に出力）:

    ```sh
    dune exec myproject -- ./example/positive_example/init_10.imp
    ```

    - busyと出る時:
    ```sh
    dune exec --build-dir="_tmp" myproject -- ./example/positive_example/init_10.imp
    ```

    - すでに所有権の検査を終えていて篩型（assert）のみの検査を行いたい場合:
    ```sh
    dune exec myproject -- ./example/positive_example/init_10.imp -refinement
    ```

    - 関数定義で行列の型アノテーションを全て記載している場合（通常と比較して速く検査が終了することが予想される）:
    ```sh
    dune exec myproject -- ./example/positive_example/init_10.imp -full_annotated
    ```

    - 所有権型推論でヒューリスティクスを用いない場合:
    ```sh
    dune exec myproject -- ./example/positive_example/init_10.imp -random_assignment
    ```

    - aliasの自動挿入を行う場合（aliasの自動挿入が正しく行われる保証はなし）:
    ```sh
    dune exec myproject -- ./example/positive_example/init_10.imp -insert_alias
    ```

- プログラム全体の構文木を文字列化して標準出力に表示:

    ```sh
    dune exec myproject -- ./example/positive_example/init_10.imp -print_program
    ```

- 特定のディレクトリにある全ての `.imp` ファイルの検査:
    ```sh
    make run DIR=./example/...
    ```

- オプションを追加したい時:
    ```sh
    make run DIR=./example/omit_alias OPTS="-insert_alias"
    ```

- `positive_example/`、`negative_example/`、`much_time_example/` にある全ての `.imp` ファイルの検査:
    ```sh
    make run_all
    ```

## その他

- `example/` 内に検証できるプログラム例をいくつか置いてあります。パス名を変えて試してみてください。
- 篩型検査にEldaricaを用いる場合、`main` の hoice 部分を以下で書き換えてください:
  ```
  <Eldaricaのパス> -hsmt ./experiment/out_chc.smt2 > experiment/chc_result
  ```
  例:
  ```
  ~/Downloads/eldarica-2.2/eld -hsmt ./experiment/out_chc.smt2
  ```

## ビルド

### 依存関係

本ツールはOCamlで記述されており、OCaml 4.14.1での動作を確認しています。
以下のOCamlライブラリに依存しています:

* zarith
* ppx_deriving
* ounit2
* seq
* unix
* stdlib-shims
* menhir

また、本ツールが生成する制約を解くために以下の外部ツールが必要です。
いずれも `$PATH` 上に配置してください。

* [z3](https://github.com/Z3Prover/z3)（4.14.1で動作確認済み）
* [hoice](https://github.com/hopv/hoice)（1.10.0で動作確認済み）

#### hoice のインストール

hoice には Rust が必要です。[rustup](https://rustup.rs/) で Rust をインストールしてください。

注意: hoice v1.10.0 は Rust 1.81 以降ではコンパイルできません。Rust 1.78.0 を使用してください:

```sh
rustup install 1.78.0
rustup run 1.78.0 cargo install --git https://github.com/hopv/hoice --tag v1.10.0 --locked
```

これにより `hoice` バイナリが `~/.cargo/bin/` に配置されます。rustup 経由で Rust をインストールした場合、このパスは自動的に `$PATH` に含まれます。

### コンパイル

コンパイルには `dune` を使用します。
以下のコマンドで実行ファイル `main` がビルドされます。
```sh
cd myproject
mkdir -p experiment/own_result
dune build
```

注: `experiment/` ディレクトリは検証中に生成される中間ファイルの保存に使用されます。ツールの実行前に作成しておく必要があります。

オブジェクトファイルと実行ファイルを削除するには、`myproject/` ディレクトリで以下を実行してください:

```
dune clean
```

## 実行方法

`z3` と `hoice` がインストールされていれば、`dune exec myproject` で実行できます。
例:

```sh
dune exec myproject -- ./example/positive_example/init_10.imp
```
`init_10.imp` のアサーション違反の不在を検査します。
`ownership: sat` と `refinement: sat` が表示されれば、それぞれ所有権推論と篩型推論が成功したことを意味します。

### オプション

| フラグ | 用途 |
|---|---|
| `-refinement` | 篩型（アサーション）検査のみ。所有権検査が完了済みの場合に使用。 |
| `-full_annotated` | 完全アノテーションモード。関数定義に行列の型アノテーションを全て記載している場合に使用。高速化が期待される。 |
| `-random_assignment` | 所有権型推論のヒューリスティクスを無効化。 |
| `-insert_alias` | aliasの自動挿入。挿入されるaliasの正しさは保証されない。 |
| `-print_program` | パースされたプログラムのASTを整形表示。 |
