# nested_array_ConSORT

[ConSORT](https://www.fos.kuis.kyoto-u.ac.jp/projects/consort/) をポインタ算術およびネストされた配列に対応するよう拡張したツールです。
カスタム `.imp` 言語で記述された命令型プログラムに対して、所有権型の自動推論と篩型の検査を行います。

[English README](README.md)

**関連リポジトリ:**
- [Extended_ConSORT (Tanaka et al.)](https://github.com/mamizu-git/Extended_ConSORT) — 先行ツール

## Docker によるクイックスタート

アーティファクト評価用のビルド済み Docker イメージが提供されています。依存関係の手動インストールは不要です。

```sh
# イメージの読み込み（tar.gz で配布されている場合）
docker load -i nested-array-consort-ecoop26.tar.gz

# またはソースからビルド
docker build --platform linux/amd64 -t nested-array-consort:ecoop26 .

# コンテナの起動
docker run -it --platform linux/amd64 nested-array-consort:ecoop26

# コンテナ内: 簡易テスト
bash eval/run_short.sh

# コンテナ内: 完全な評価（論文の Table 1-3 を再現）
bash eval/run_all.sh
```

詳細な評価手順は [ARTIFACT.md](ARTIFACT.md) を参照してください。

## ソースからのビルド

### 依存関係

本ツールは OCaml で記述されており、OCaml 4.14.1 での動作を確認しています（4.13 以上が必要）。
以下の OCaml ライブラリに依存しています:

* zarith
* ppx_deriving
* ounit2
* menhir

また、本ツールが生成する制約を解くために以下の外部ソルバが必要です。
いずれも `$PATH` 上に配置してください。

* [Z3](https://github.com/Z3Prover/z3)（4.14.1 で動作確認済み）
* [hoice](https://github.com/hopv/hoice)（1.10.0 で動作確認済み）

#### hoice のインストール

hoice には Rust が必要です。[rustup](https://rustup.rs/) で Rust をインストールしてください。

注意: hoice v1.10.0 は Rust 1.81 以降ではコンパイルできません。Rust 1.78.0 を使用してください:

```sh
rustup install 1.78.0
rustup run 1.78.0 cargo install --git https://github.com/hopv/hoice --tag v1.10.0 --locked
```

これにより `hoice` バイナリが `~/.cargo/bin/` に配置されます。rustup 経由で Rust をインストールした場合、このパスは自動的に `$PATH` に含まれます。

### コンパイル

以下のコマンドはすべて `myproject/` ディレクトリ下で実行してください。

```sh
cd myproject
mkdir -p experiment/own_result
dune build
```

注: `experiment/` ディレクトリは検証中に生成される中間ファイルの保存に使用されます。ツールの実行前に作成しておく必要があります。

ビルド成果物を削除するには:

```sh
dune clean
```

## 使い方

以下のコマンドはすべて `myproject/` ディレクトリ下で実行してください。

### 基本的な検証

`.imp` ファイルに対して所有権推論と篩型検査を実行:

```sh
dune exec myproject -- ./example/positive_example/init_10.imp
```

`ownership: sat` と `refinement: sat` が表示されれば、それぞれ所有権推論と篩型推論が成功したことを意味します。結果は `experiment/out_sat_ans.smt2` に出力されます。

busy と出る時:
```sh
dune exec --build-dir="_tmp" myproject -- ./example/positive_example/init_10.imp
```

### CLI オプション

| フラグ | 用途 |
|---|---|
| `-refinement` | 篩型（アサーション）検査のみ。所有権検査が完了済みの場合に使用。 |
| `-full_annotated` | 完全アノテーションモード。関数定義に行列の型アノテーションを全て記載している場合に使用。高速化が期待される。 |
| `-random_assignment` | 所有権型推論のヒューリスティクスを無効化。 |
| `-insert_alias` | alias の自動挿入。挿入される alias の正しさは保証されない。 |
| `-print_program` | パースされたプログラムの AST を整形表示。 |
| `-unsat-core true/false` | unsat core 解析の有効/無効（デフォルト: true）。 |

### バッチ検証

```sh
# 特定のディレクトリにある全ての .imp ファイルの検査
make run DIR=./example/positive_example

# オプションを追加したい時
make run DIR=./example/omit_alias OPTS="-insert_alias"

# positive_example/、negative_example/、much_time_example/、omit_alias/ にある
# 全ての .imp ファイルの検査
make run_all
```

### アーティファクト評価

ECOOP 2026 アーティファクト評価の詳細な手順は [ARTIFACT.md](ARTIFACT.md) を参照してください。

```sh
make eval-short     # 簡易テスト（ベンチマークのサブセット）
make eval-all       # 完全な評価: 論文の全テーブルを再現
make eval-table1    # Table 1 の再現（ネスト配列ベンチマーク）
make eval-table3    # Table 3 の再現（Tanaka et al. との比較）
make eval-table4    # Table 4 の再現（Z3 バージョン比較）
make eval-table5    # Table 5 の再現（Z3 バージョン比較サマリ）
```

## プログラム例

テスト用の `.imp` ファイルが `myproject/example/` に用意されています:

| ディレクトリ | 説明 |
|---|---|
| `nested_arrays/` | ネスト配列ベンチマーク（論文 Table 1） |
| `int_arrays/` | 整数配列ベンチマーク（論文 Table 3） |
| `benchmark_nested_array_in_paper/` | 論文に掲載されたベンチマークのサブセット |
| `positive_example/` | 検証に成功するプログラム |
| `negative_example/` | 検証に失敗するプログラム |
| `much_time_example/` | 検証に時間がかかるケース |
| `omit_alias/` | alias 注釈なしの例（`-insert_alias` と併用） |

## その他

- 篩型検査に [Eldarica](https://github.com/uuverifiers/eldarica) を用いる場合、`main` の hoice 部分を以下で書き換えてください:
  ```
  <Eldaricaのパス> -hsmt ./experiment/out_chc.smt2 > experiment/chc_result
  ```

## リポジトリ構成

```
nested_array_ConSORT/
  ARTIFACT.md              -- アーティファクト評価ドキュメント
  Dockerfile               -- アーティファクト評価用 Docker イメージ
  build-artifact.sh        -- Docker イメージのビルド・パッケージスクリプト
  LICENSE                  -- Apache License 2.0
  darts/                   -- DARTS アーティファクト記述（LaTeX）
  verus/                   -- Verus 検証コード
  myproject/               -- メインツールのソースコード
    bin/main.ml            -- エントリポイント（CLI 引数パース）
    lib/                   -- コアライブラリモジュール
    example/               -- ベンチマークプログラム（.imp ファイル）
    eval/                  -- アーティファクト再現用評価スクリプト
    experiment/            -- 中間ソルバファイル（実行時に生成）
```

## ライセンス

本プロジェクトは Apache License 2.0 の下でライセンスされています。詳細は [LICENSE](LICENSE) を参照してください。
