#!/usr/bin/env bash
# setup.sh — Install dependencies and build tools for artifact evaluation
set -euo pipefail

EVAL_DIR="$(cd "$(dirname "$0")" && pwd)"
MYPROJECT_DIR="$(cd "$EVAL_DIR/.." && pwd)"
Z3_VERSIONS_DIR="$EVAL_DIR/z3-versions"

echo "============================================================"
echo "  Artifact Evaluation Setup"
echo "============================================================"

# ---- Check prerequisites ----
echo ""
echo "--- Checking prerequisites ---"

check_cmd() {
    if command -v "$1" >/dev/null 2>&1; then
        echo "  [OK] $1 found: $(command -v "$1")"
        return 0
    else
        echo "  [MISSING] $1 not found"
        return 1
    fi
}

MISSING=0
check_cmd ocaml || MISSING=1
check_cmd dune || MISSING=1
check_cmd python3 || MISSING=1

if ! check_cmd cargo; then
    echo "         Install Rust via https://rustup.rs/ for hoice support"
    MISSING=1
fi

if [ $MISSING -ne 0 ]; then
    echo ""
    echo "ERROR: Missing prerequisites. Install them and re-run setup.sh."
    exit 1
fi

# ---- Build this tool ----
echo ""
echo "--- Building nested_array_ConSORT ---"
cd "$MYPROJECT_DIR"
mkdir -p experiment/own_result
dune build
echo "  [OK] Build succeeded"

# ---- Install hoice ----
echo ""
echo "--- Checking hoice ---"
if command -v hoice >/dev/null 2>&1; then
    echo "  [OK] hoice found: $(command -v hoice)"
else
    echo "  hoice not found. Installing via cargo..."
    cargo install --git https://github.com/hopv/hoice
    if command -v hoice >/dev/null 2>&1; then
        echo "  [OK] hoice installed"
    else
        echo "  ERROR: hoice installation failed"
        exit 1
    fi
fi

# ---- Download Z3 versions ----
echo ""
echo "--- Setting up Z3 versions ---"
mkdir -p "$Z3_VERSIONS_DIR"

download_z3() {
    local version="$1"
    local target_dir="$Z3_VERSIONS_DIR/z3-${version}"

    if [ -x "$target_dir/bin/z3" ]; then
        echo "  [OK] Z3 $version already installed"
        return 0
    fi

    echo "  Downloading Z3 $version..."

    local os arch asset_name
    os=$(uname -s)
    arch=$(uname -m)

    case "$os" in
        Darwin)
            case "$version" in
                4.14.1)
                    case "$arch" in
                        arm64)  asset_name="z3-4.14.1-arm64-osx-13.7.4.zip" ;;
                        x86_64) asset_name="z3-4.14.1-x64-osx-13.7.4.zip" ;;
                    esac ;;
                4.11.2)
                    case "$arch" in
                        arm64)  asset_name="z3-4.11.2-arm64-osx-11.0.zip" ;;
                        x86_64) asset_name="z3-4.11.2-x64-osx-10.16.zip" ;;
                    esac ;;
            esac ;;
        Linux)
            case "$version" in
                4.14.1)
                    case "$arch" in
                        aarch64) asset_name="z3-4.14.1-arm64-glibc-2.34.zip" ;;
                        x86_64)  asset_name="z3-4.14.1-x64-glibc-2.35.zip" ;;
                    esac ;;
                4.11.2)
                    case "$arch" in
                        x86_64) asset_name="z3-4.11.2-x64-glibc-2.31.zip" ;;
                        *)
                            echo "  ERROR: No Z3 $version pre-built binary for Linux $arch."
                            echo "         Build from source: https://github.com/Z3Prover/z3"
                            return 1 ;;
                    esac ;;
            esac ;;
        *)
            echo "  ERROR: Unsupported OS: $os"
            return 1 ;;
    esac

    if [ -z "${asset_name:-}" ]; then
        echo "  ERROR: Could not determine Z3 asset for $os/$arch/$version"
        return 1
    fi

    local url="https://github.com/Z3Prover/z3/releases/download/z3-${version}/${asset_name}"
    local tmp_zip="$Z3_VERSIONS_DIR/${asset_name}"

    curl -L -o "$tmp_zip" "$url"

    # Extract — the zip contains a top-level directory
    local tmp_extract="$Z3_VERSIONS_DIR/tmp_extract_$$"
    mkdir -p "$tmp_extract"
    unzip -q "$tmp_zip" -d "$tmp_extract"

    # Find the extracted directory (name varies)
    local extracted_dir
    extracted_dir=$(find "$tmp_extract" -maxdepth 1 -mindepth 1 -type d | head -1)

    mv "$extracted_dir" "$target_dir"
    rm -rf "$tmp_extract" "$tmp_zip"

    chmod +x "$target_dir/bin/z3"
    echo "  [OK] Z3 $version installed to $target_dir"
}

download_z3 "4.14.1"
download_z3 "4.11.2"

# ---- Clone and build Extended_ConSORT ----
echo ""
echo "--- Setting up Extended_ConSORT (Tanaka et al.) ---"
EXTENDED_CONSORT_DIR="$EVAL_DIR/Extended_ConSORT"

if [ -d "$EXTENDED_CONSORT_DIR" ]; then
    echo "  [OK] Extended_ConSORT already cloned"
else
    echo "  Cloning Extended_ConSORT..."
    git clone https://github.com/mamizu-git/Extended_ConSORT "$EXTENDED_CONSORT_DIR"
fi

if [ -x "$EXTENDED_CONSORT_DIR/src/main" ]; then
    echo "  [OK] Extended_ConSORT already built"
else
    echo "  Building Extended_ConSORT..."
    (cd "$EXTENDED_CONSORT_DIR/src" && make)
    echo "  [OK] Extended_ConSORT built"
fi

mkdir -p "$EXTENDED_CONSORT_DIR/experiment"

# ---- Smoke test ----
echo ""
echo "--- Running smoke test ---"

source "$EVAL_DIR/lib/common.sh"

use_z3 "4.14.1"
output=$(run_tool "./example/nested_arrays/indexed_value.imp")
own=$(parse_own_time "$output")
ref=$(parse_ref_time "$output")
total=$(parse_total_time "$output")

if [ -n "$own" ] && [ -n "$ref" ] && [ -n "$total" ]; then
    echo "  [OK] Smoke test passed (Z3 4.14.1): own=${own}s ref=${ref}s total=${total}s"
else
    echo "  WARNING: Smoke test output parsing issue. Raw output:"
    echo "$output"
fi

use_z3 "4.11.2"
output=$(run_tool "./example/nested_arrays/indexed_value.imp")
own=$(parse_own_time "$output")
if [ -n "$own" ]; then
    echo "  [OK] Smoke test passed (Z3 4.11.2): own=${own}s"
else
    echo "  WARNING: Smoke test output parsing issue with Z3 4.11.2"
fi

echo ""
echo "============================================================"
echo "  Setup complete!"
echo "  Run ./run_all.sh or individual table scripts."
echo "============================================================"
