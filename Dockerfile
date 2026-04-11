# =============================================================================
# Dockerfile for nested_array_ConSORT — ECOOP 2026 Artifact Evaluation
# =============================================================================
# Base image: Ubuntu 22.04 with OCaml 4.14 (opam pre-configured)
# Supports both x86_64 (amd64) and ARM64 (aarch64/Apple Silicon) platforms.
FROM ocaml/opam:ubuntu-22.04-ocaml-4.14

USER opam
WORKDIR /home/opam/app

# =============================================================================
# 1. System dependencies
# =============================================================================
RUN sudo apt-get update && sudo apt-get install -y \
    libgmp-dev \
    pkg-config \
    python3 \
    wget \
    unzip \
    git \
    cmake \
    g++ \
    && sudo rm -rf /var/lib/apt/lists/*

# =============================================================================
# 2. Z3 SMT Solver (two versions for evaluation)
#    - 4.14.1: primary solver used by nested_array_ConSORT
#    - 4.11.2: used by Extended_ConSORT for comparison
#
#    On amd64: pre-built binaries are used for both versions.
#    On arm64: 4.14.1 uses pre-built arm64 binary; 4.11.2 is built from
#    source via opam (no pre-built arm64 release exists for 4.11.2).
# =============================================================================

ARG TARGETARCH

# Z3 4.14.1 (default) — pre-built binaries available for both amd64 and arm64
RUN if [ "$TARGETARCH" = "arm64" ]; then \
      Z3_ARCHIVE="z3-4.14.1-arm64-glibc-2.35"; \
    else \
      Z3_ARCHIVE="z3-4.14.1-x64-glibc-2.35"; \
    fi && \
    wget -q "https://github.com/Z3Prover/z3/releases/download/z3-4.14.1/${Z3_ARCHIVE}.zip" && \
    unzip -q "${Z3_ARCHIVE}.zip" && \
    sudo mkdir -p /usr/local/z3-4.14.1/bin && \
    sudo cp "${Z3_ARCHIVE}/bin/z3" /usr/local/z3-4.14.1/bin/z3 && \
    sudo chmod +x /usr/local/z3-4.14.1/bin/z3 && \
    sudo ln -sf /usr/local/z3-4.14.1/bin/z3 /usr/local/bin/z3 && \
    rm -rf ${Z3_ARCHIVE}*

# Z3 4.11.2 (for Extended_ConSORT comparison)
#   arm64: built from source via opam (install, copy binary, remove)
#   amd64: pre-built binary
RUN if [ "$TARGETARCH" = "arm64" ]; then \
      opam install z3.4.11.2 -y && \
      sudo mkdir -p /usr/local/z3-4.11.2/bin && \
      sudo cp "$(opam var bin)/z3" /usr/local/z3-4.11.2/bin/z3 && \
      sudo chmod +x /usr/local/z3-4.11.2/bin/z3 && \
      opam remove z3 -y; \
    else \
      wget -q https://github.com/Z3Prover/z3/releases/download/z3-4.11.2/z3-4.11.2-x64-glibc-2.31.zip && \
      unzip -q z3-4.11.2-x64-glibc-2.31.zip && \
      sudo mkdir -p /usr/local/z3-4.11.2/bin && \
      sudo cp z3-4.11.2-x64-glibc-2.31/bin/z3 /usr/local/z3-4.11.2/bin/z3 && \
      sudo chmod +x /usr/local/z3-4.11.2/bin/z3 && \
      rm -rf z3-4.11.2-x64-glibc-2.31*; \
    fi

# =============================================================================
# 3. Rust 1.78.0 and Hoice CHC solver (v1.10.0)
#    Rust 1.78.0 is required — newer versions cause build failures with hoice.
# =============================================================================
RUN wget -qO- https://sh.rustup.rs | sh -s -- -y --default-toolchain 1.78.0
ENV PATH="/home/opam/.cargo/bin:${PATH}"
RUN cargo install --git https://github.com/hopv/hoice --tag v1.10.0 --locked

# =============================================================================
# 4. Extended_ConSORT (baseline for comparison in evaluation)
# =============================================================================
RUN mkdir -p /home/opam/app/eval \
    && git clone https://github.com/mamizu-git/Extended_ConSORT /home/opam/app/eval/Extended_ConSORT \
    && cd /home/opam/app/eval/Extended_ConSORT/src && opam exec -- make \
    && mkdir -p /home/opam/app/eval/Extended_ConSORT/experiment

# =============================================================================
# 5. Copy source code and build
#    The opam file is at myproject/myproject.opam, so we copy everything
#    then install from myproject/.
# =============================================================================
COPY --chown=opam:opam . .
WORKDIR /home/opam/app/myproject

# Create required directories for solver output
RUN mkdir -p experiment/own_result

# Install OCaml dependencies and build the project
RUN opam install . --deps-only -y
RUN opam exec -- dune build

# =============================================================================
# 6. Set up eval environment
#    Create symlinks so eval scripts can find Z3 versions and Extended_ConSORT
#    at the expected paths relative to myproject/eval/.
# =============================================================================
RUN mkdir -p eval/z3-versions \
    && ln -sf /usr/local/z3-4.14.1 eval/z3-versions/z3-4.14.1 \
    && ln -sf /usr/local/z3-4.11.2 eval/z3-versions/z3-4.11.2 \
    && ln -sf /home/opam/app/eval/Extended_ConSORT eval/Extended_ConSORT

# =============================================================================
# 7. Default working directory and entrypoint
# =============================================================================
WORKDIR /home/opam/app/myproject
CMD ["bash"]
