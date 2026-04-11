#!/usr/bin/env bash
# common.sh — Shared functions for table reproduction scripts
# Compatible with bash 3.2+ (no associative arrays)

set -euo pipefail

# ---- Directory Setup ----
EVAL_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
MYPROJECT_DIR="$(cd "$EVAL_DIR/.." && pwd)"
Z3_VERSIONS_DIR="$EVAL_DIR/z3-versions"
EXTENDED_CONSORT_DIR="$EVAL_DIR/Extended_ConSORT"
RESULTS_DIR="$EVAL_DIR/results"

mkdir -p "$RESULTS_DIR"
mkdir -p "$MYPROJECT_DIR/experiment/own_result"

# ---- Timeout Command Detection ----
if command -v timeout >/dev/null 2>&1; then
    TIMEOUT_CMD=timeout
elif command -v gtimeout >/dev/null 2>&1; then
    TIMEOUT_CMD=gtimeout
else
    echo "WARNING: neither timeout nor gtimeout found. Running without timeout." >&2
    TIMEOUT_CMD=""
fi

TIMEOUT_SEC="${TIMEOUT_SEC:-600}"

# ---- Z3 Version Switching ----
_ORIGINAL_PATH="${_ORIGINAL_PATH:-$PATH}"

use_z3() {
    local version="$1"
    local z3_bin="$Z3_VERSIONS_DIR/z3-${version}/bin"
    if [ ! -x "$z3_bin/z3" ]; then
        echo "ERROR: Z3 $version not found at $z3_bin/z3. Run setup.sh first." >&2
        return 1
    fi
    export PATH="$z3_bin:$_ORIGINAL_PATH"
    local actual
    actual=$(z3 --version 2>/dev/null | grep -oE '[0-9]+\.[0-9]+\.[0-9]+' | head -1)
    if [ "$actual" != "$version" ]; then
        echo "ERROR: Expected Z3 $version but got $actual" >&2
        return 1
    fi
}

# ---- Run Our Tool ----
run_tool() {
    local imp_file="$1"
    shift
    if [ -n "$TIMEOUT_CMD" ]; then
        (cd "$MYPROJECT_DIR" && $TIMEOUT_CMD "${TIMEOUT_SEC}s" dune exec myproject -- "$imp_file" -unsat-core false "$@" 2>&1) || true
    else
        (cd "$MYPROJECT_DIR" && dune exec myproject -- "$imp_file" -unsat-core false "$@" 2>&1) || true
    fi
}

# ---- Run Extended_ConSORT (Tanaka et al.) with Timing ----
run_tanaka() {
    local imp_file="$1"
    local consort_dir="$EXTENDED_CONSORT_DIR"
    local j=3

    (
        cd "$consort_dir"
        rm -f experiment/*

        local start_ns own_end_ns ref_end_ns

        start_ns=$(python3 -c 'import time; print(time.time())')

        # Ownership phase: iterative int + fv loop
        while true; do
            ./src/main int "$imp_file" $j 2>/dev/null
            z3 experiment/out_int.smt2 > experiment/result_int 2>/dev/null
            ./src/main fv "$imp_file" 0 2>/dev/null
            z3 experiment/out_fv.smt2 > experiment/result 2>/dev/null
            local st
            st=$(head -n 1 experiment/result)
            if [ "$st" = "sat" ]; then
                break
            fi
            j=$((j + 1))
            if [ $j -gt 100 ]; then
                echo "TIMEOUT 0 0 unknown unknown"
                return
            fi
        done

        own_end_ns=$(python3 -c 'import time; print(time.time())')

        # Refinement phase
        ./src/main chc "$imp_file" 0 2>/dev/null
        hoice experiment/out_chc.smt2 > experiment/chc_result 2>/dev/null
        local st2
        st2=$(head -n 1 experiment/chc_result)

        ref_end_ns=$(python3 -c 'import time; print(time.time())')

        local own_time ref_time total_time
        own_time=$(python3 -c "print(f'{$own_end_ns - $start_ns:.3f}')")
        ref_time=$(python3 -c "print(f'{$ref_end_ns - $own_end_ns:.3f}')")
        total_time=$(python3 -c "print(f'{$ref_end_ns - $start_ns:.3f}')")

        echo "$own_time $ref_time $total_time sat $st2"
    )
}

# ---- Output Parsing ----
parse_own_time() {
    echo "$1" | grep -oE 'own time:? *[0-9]+\.[0-9]+' | grep -oE '[0-9]+\.[0-9]+' | tail -1
}

parse_ref_time() {
    echo "$1" | grep -oE 'ref time [0-9]+\.[0-9]+' | grep -oE '[0-9]+\.[0-9]+' | tail -1
}

parse_total_time() {
    echo "$1" | grep -oE 'total time: [0-9]+\.[0-9]+' | grep -oE '[0-9]+\.[0-9]+' | tail -1
}

parse_ownership_result() {
    echo "$1" | grep -oE 'ownership: (sat|unsat)' | awk '{print $2}' | tail -1
}

parse_refinement_result() {
    echo "$1" | grep -oE 'refinement: (sat|unsat)' | awk '{print $2}' | tail -1
}

# ---- Alias Counting ----
count_aliases() {
    local imp_file="$1"
    shift
    local output
    output=$(cd "$MYPROJECT_DIR" && dune exec myproject -- "$imp_file" -print_program "$@" 2>&1)
    echo "$output" | grep -oE "AliasAddPtr|AliasDeref" | wc -l | tr -d ' '
}

# ---- Benchmark Name Mapping (bash 3.2 compatible) ----
# Returns the .imp filename (without extension) for a paper name
get_nested_array_file() {
    case "$1" in
        "Init-Matrix(2D)")      echo "initMatrix" ;;
        "Init-Matrix(3D)")      echo "initThreeMatrix" ;;
        "Indexed-Matrix(2D)")   echo "indexed_array" ;;
        "Indexed-Matrix(3D)")   echo "indexedThreeMatrix" ;;
        "Indexed-Matrix(4D)")   echo "indexedFourMatrix" ;;
        "Indexed-Value")        echo "indexed_value" ;;
        "Sum-Matrix")           echo "sum_matrix" ;;
        "Copy-Matrix")          echo "copy_matrix" ;;
        "Add-Matrix")           echo "add_matrix" ;;
        "Trace-Matrix")         echo "trace" ;;
        "Trans-Matrix")         echo "trans" ;;
        "Eta-Equ-Sum")          echo "eta_equi_sum_matrix" ;;
        "Eta-Equ-Trace")        echo "eta_equi_trace" ;;
        "Lower-Triangle")       echo "lower_triangle_matrix" ;;
        "Swap")                 echo "swap" ;;
        "Compare-Element")      echo "compare_element" ;;
        "Row-Add")              echo "rowadd" ;;
        "Boomerang")            echo "boomerang_array" ;;
        "Share-Add-Matrix")     echo "share_add_matrix" ;;
        *) echo "UNKNOWN"; return 1 ;;
    esac
}

get_int_array_file() {
    case "$1" in
        "Init-10")      echo "init_10" ;;
        "Init-1000")    echo "init" ;;
        "Sum")          echo "sum" ;;
        "Sum-Back")     echo "sum_back" ;;
        "Sum-Both")     echo "sum_both" ;;
        "Sum-Div")      echo "sum_div" ;;
        "Copy-Array")   echo "copy_array" ;;
        "Add-Array")    echo "add_array" ;;
        *) echo "UNKNOWN"; return 1 ;;
    esac
}

get_omit_alias_name() {
    case "$1" in
        boomerang_array) echo "boomerang" ;;
        *) echo "$1" ;;
    esac
}

# Ordered benchmark lists (space-separated, parentheses escaped for iteration)
# Use read_nested_array_order and read_int_array_order functions
read_nested_array_order() {
    cat << 'EOF'
Init-Matrix(2D)
Init-Matrix(3D)
Indexed-Matrix(2D)
Indexed-Matrix(3D)
Indexed-Matrix(4D)
Indexed-Value
Sum-Matrix
Copy-Matrix
Add-Matrix
Trace-Matrix
Trans-Matrix
Eta-Equ-Sum
Eta-Equ-Trace
Lower-Triangle
Swap
Compare-Element
Row-Add
Boomerang
Share-Add-Matrix
EOF
}

read_int_array_order() {
    cat << 'EOF'
Init-10
Init-1000
Sum
Sum-Back
Sum-Both
Sum-Div
Copy-Array
Add-Array
EOF
}

# ---- Formatting Helpers ----
log_progress() {
    echo "[$(date '+%H:%M:%S')] $*"
}
