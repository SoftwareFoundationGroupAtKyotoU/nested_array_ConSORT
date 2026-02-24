# 1. OCamlのバージョンとOSを指定
FROM ocaml/opam:ubuntu-22.04-ocaml-4.14

# ★Z3のバージョンとアーキテクチャ（x64に修正。ARMなら arm64 に書き換えてください）
ARG Z3_VERSION=4.14.0
ARG Z3_ARCH=x64-glibc-2.35

USER opam
WORKDIR /home/opam/app

# 2. システム依存ライブラリのインストール
RUN sudo apt-get update && sudo apt-get install -y \
    libgmp-dev \
    pkg-config \
    python3 \
    wget \
    unzip \
    && sudo rm -rf /var/lib/apt/lists/*

# 3. Z3のインストール
# URLが正しいか注意（例: 4.14.1は現在執筆時点で最新すぎると存在しない場合があるため、4.13.0等が安定）
RUN wget -q https://github.com/Z3Prover/z3/releases/download/z3-4.14.1/z3-4.14.1-arm64-glibc-2.34.zip \
    && unzip -q z3-4.14.1-arm64-glibc-2.34.zip \
    && sudo cp z3-4.14.1-arm64-glibc-2.34/bin/z3 /usr/local/bin/z3 \
    && sudo chmod +x /usr/local/bin/z3 \
    && rm -rf z3-4.14.1-arm64-glibc-2.34*

# 4. Rust & Hoice のインストール
# rustupを先にインストールし、PATHを通してから cargo install
RUN wget -qO- https://sh.rustup.rs | sh -s -- -y
ENV PATH="/home/opam/.cargo/bin:${PATH}"
RUN cargo install --git https://github.com/hopv/hoice --tag v1.10.0 --locked

# 5. OCaml依存ライブラリのインストール
# プロジェクト全体をコピーする前に、依存関係定義ファイルだけコピーするとビルドが速くなります
COPY --chown=opam:opam *.opam ./


# 6. ソースコードをコピーしてビルド
COPY --chown=opam:opam . .
# myprojectディレクトリへ移動が必要な場合はここで行う
WORKDIR /home/opam/app/myproject

RUN opam install . --deps-only
RUN opam exec -- dune build

CMD ["bash"]