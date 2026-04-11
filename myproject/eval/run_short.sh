#!/usr/bin/env bash
# run_short.sh — "Kick-the-tires" script for ECOOP artifact evaluation
# Runs a subset of benchmarks from Tables 1-4 in under 10 minutes.
set -euo pipefail

EVAL_DIR="$(cd "$(dirname "$0")" && pwd)"
source "$EVAL_DIR/lib/common.sh"

# Use Z3 4.14.1 as default
use_z3 "4.14.1"

echo "============================================================"
echo "  Kick-the-Tires: Quick Verification (subset of Tables 1, 3, 4)"
echo "  Expected runtime: < 10 minutes"
echo "============================================================"
echo ""

SHORT_DIR="$RESULTS_DIR/short_run"
mkdir -p "$SHORT_DIR"

PASS_COUNT=0
FAIL_COUNT=0
SUMMARY=""

record_result() {
    local name="$1"
    local status="$2"
    local detail="$3"
    if [ "$status" = "OK" ]; then
        PASS_COUNT=$((PASS_COUNT + 1))
        SUMMARY="${SUMMARY}  [PASS] ${name}: ${detail}\n"
    else
        FAIL_COUNT=$((FAIL_COUNT + 1))
        SUMMARY="${SUMMARY}  [FAIL] ${name}: ${detail}\n"
    fi
}

# ==================================================================
# Table 1 subset: Run 3 fast benchmarks
# ==================================================================
echo "------------------------------------------------------------"
echo "  Table 1 (subset): Nested Array Benchmarks"
echo "------------------------------------------------------------"

TABLE1_BENCHMARKS="Init-Matrix(2D)
Indexed-Value
Swap"

TABLE1_CSV="$SHORT_DIR/table1_subset.csv"
echo "Name,Total,Ownership,Refinement,Manual_Alias,Auto_Alias,Total_Alias" > "$TABLE1_CSV"

while IFS= read -r paper_name; do
    file_base=$(get_nested_array_file "$paper_name")
    imp_file="./example/nested_arrays/${file_base}.imp"
    omit_base=$(get_omit_alias_name "$file_base")
    omit_file="./example/omit_alias/${omit_base}.imp"

    log_progress "Table 1: Running $paper_name ($file_base.imp)..."

    # Run verification
    output=$(run_tool "$imp_file")

    own_time=$(parse_own_time "$output")
    ref_time=$(parse_ref_time "$output")
    total_time=$(parse_total_time "$output")

    if [ -z "$total_time" ]; then
        record_result "Table1/$paper_name" "FAIL" "Could not parse timing output"
        echo "$paper_name,ERROR,ERROR,ERROR,,,," >> "$TABLE1_CSV"
        continue
    fi

    # Count aliases
    manual_alias=$(count_aliases "$omit_file")
    total_with_insert=$(count_aliases "$omit_file" "-insert_alias")
    auto_alias=$((total_with_insert - manual_alias))
    total_alias=$((manual_alias + auto_alias))

    echo "$paper_name,$total_time,$own_time,$ref_time,$manual_alias,$auto_alias,$total_alias" >> "$TABLE1_CSV"

    record_result "Table1/$paper_name" "OK" \
        "total=${total_time}s (own=${own_time}s+ref=${ref_time}s), alias: manual=$manual_alias auto=$auto_alias total=$total_alias"

    log_progress "  -> total=${total_time}s, alias: manual=$manual_alias auto=$auto_alias total=$total_alias"
done <<< "$TABLE1_BENCHMARKS"

echo ""

# ==================================================================
# Table 3 subset: Run Init-10 with all 3 configurations
# ==================================================================
echo "------------------------------------------------------------"
echo "  Table 3 (subset): Comparison with Tanaka et al."
echo "------------------------------------------------------------"

TABLE3_CSV="$SHORT_DIR/table3_subset.csv"
echo "Name,Tanaka_4.11.2_Total,Tanaka_4.11.2_Own,Tanaka_4.11.2_Ref,Ours_4.11.2_Total,Ours_4.11.2_Own,Ours_4.11.2_Ref,Ours_4.14.1_Total,Ours_4.14.1_Own,Ours_4.14.1_Ref" > "$TABLE3_CSV"

paper_name="Init-10"
file_base=$(get_int_array_file "$paper_name")

log_progress "Table 3: Running $paper_name — Tanaka+ (Z3 4.11.2)..."
use_z3 "4.11.2"
tanaka_output=$(run_tanaka "examples/non-alias/${file_base}.imp" 2>&1) || true

tanaka_own=$(echo "$tanaka_output" | awk '{print $1}')
tanaka_ref=$(echo "$tanaka_output" | awk '{print $2}')
tanaka_total=$(echo "$tanaka_output" | awk '{print $3}')

log_progress "Table 3: Running $paper_name — Ours (Z3 4.11.2)..."
output_411=$(run_tool "./example/int_arrays/${file_base}.imp")

ours_411_own=$(parse_own_time "$output_411")
ours_411_ref=$(parse_ref_time "$output_411")
ours_411_total=$(parse_total_time "$output_411")

log_progress "Table 3: Running $paper_name — Ours (Z3 4.14.1)..."
use_z3 "4.14.1"
output_414=$(run_tool "./example/int_arrays/${file_base}.imp")

ours_414_own=$(parse_own_time "$output_414")
ours_414_ref=$(parse_ref_time "$output_414")
ours_414_total=$(parse_total_time "$output_414")

# Defaults for missing values
tanaka_own="${tanaka_own:-TIMEOUT}"
tanaka_ref="${tanaka_ref:-TIMEOUT}"
tanaka_total="${tanaka_total:-TIMEOUT}"
ours_411_own="${ours_411_own:-TIMEOUT}"
ours_411_ref="${ours_411_ref:-TIMEOUT}"
ours_411_total="${ours_411_total:-TIMEOUT}"
ours_414_own="${ours_414_own:-TIMEOUT}"
ours_414_ref="${ours_414_ref:-TIMEOUT}"
ours_414_total="${ours_414_total:-TIMEOUT}"

echo "$paper_name,$tanaka_total,$tanaka_own,$tanaka_ref,$ours_411_total,$ours_411_own,$ours_411_ref,$ours_414_total,$ours_414_own,$ours_414_ref" >> "$TABLE3_CSV"

if [ "$ours_414_total" != "TIMEOUT" ]; then
    record_result "Table3/$paper_name/Tanaka+(4.11.2)" "OK" "total=${tanaka_total}s"
    record_result "Table3/$paper_name/Ours(4.11.2)" "OK" "total=${ours_411_total}s"
    record_result "Table3/$paper_name/Ours(4.14.1)" "OK" "total=${ours_414_total}s"
    log_progress "  -> Tanaka+: ${tanaka_total}s, Ours(4.11.2): ${ours_411_total}s, Ours(4.14.1): ${ours_414_total}s"
else
    record_result "Table3/$paper_name" "FAIL" "One or more configurations timed out"
fi

echo ""

# ==================================================================
# Table 4 subset: Run Init-Matrix(2D) and Indexed-Value with NUM_RUNS=2
# ==================================================================
echo "------------------------------------------------------------"
echo "  Table 4 (subset): Mean Verification Time (2 runs)"
echo "------------------------------------------------------------"

NUM_RUNS=2
TABLE4_DIR="$SHORT_DIR/table4_raw"
TABLE4_CSV="$SHORT_DIR/table4_subset.csv"
mkdir -p "$TABLE4_DIR"

echo "Benchmark,Own_4.14.1,Own_4.11.2,Total_4.14.1,Total_4.11.2" > "$TABLE4_CSV"

TABLE4_BENCHMARKS="Init-Matrix(2D)
Indexed-Value"

while IFS= read -r paper_name; do
    file_base=$(get_nested_array_file "$paper_name")
    imp_file="./example/nested_arrays/${file_base}.imp"
    dir_name=$(echo "$paper_name" | tr '()' '__' | tr ' ' '_')

    log_progress "Table 4: Running $paper_name ($NUM_RUNS runs x 2 Z3 versions)..."

    for z3_ver in 4.14.1 4.11.2; do
        run_dir="$TABLE4_DIR/${dir_name}/z3-${z3_ver}"
        mkdir -p "$run_dir"

        use_z3 "$z3_ver"

        for run_num in $(seq 1 "$NUM_RUNS"); do
            output=$(run_tool "$imp_file")
            own=$(parse_own_time "$output")
            ref=$(parse_ref_time "$output")
            total=$(parse_total_time "$output")
            echo "own=${own:-TIMEOUT} ref=${ref:-TIMEOUT} total=${total:-TIMEOUT}" > "$run_dir/run_${run_num}.txt"
        done
    done

    # Compute means with Python
    mean_result=$(python3 - "$TABLE4_DIR/${dir_name}" "$NUM_RUNS" << 'PYTHON_SCRIPT'
import sys, os, re

base_dir = sys.argv[1]
num_runs = int(sys.argv[2])

def parse_run_file(path, field):
    try:
        with open(path) as f:
            content = f.read().strip()
        m = re.search(rf'{field}=(\S+)', content)
        if m:
            val = m.group(1)
            if val == "TIMEOUT":
                return None
            return float(val)
    except Exception:
        return None
    return None

def compute_mean(run_dir, field, num_runs):
    vals = []
    for i in range(1, num_runs + 1):
        path = os.path.join(run_dir, f"run_{i}.txt")
        v = parse_run_file(path, field)
        if v is None:
            return None
        vals.append(v)
    if not vals:
        return None
    return sum(vals) / len(vals)

def fmt(v):
    return f"{v:.3f}" if v is not None else "TIMEOUT"

own_414 = compute_mean(os.path.join(base_dir, "z3-4.14.1"), "own", num_runs)
own_411 = compute_mean(os.path.join(base_dir, "z3-4.11.2"), "own", num_runs)
total_414 = compute_mean(os.path.join(base_dir, "z3-4.14.1"), "total", num_runs)
total_411 = compute_mean(os.path.join(base_dir, "z3-4.11.2"), "total", num_runs)

print(f"{fmt(own_414)} {fmt(own_411)} {fmt(total_414)} {fmt(total_411)}")
PYTHON_SCRIPT
    )

    own_414=$(echo "$mean_result" | awk '{print $1}')
    own_411=$(echo "$mean_result" | awk '{print $2}')
    total_414=$(echo "$mean_result" | awk '{print $3}')
    total_411=$(echo "$mean_result" | awk '{print $4}')

    echo "$paper_name,$own_414,$own_411,$total_414,$total_411" >> "$TABLE4_CSV"

    if [ "$total_414" != "TIMEOUT" ] && [ "$total_411" != "TIMEOUT" ]; then
        record_result "Table4/$paper_name" "OK" \
            "mean total: Z3 4.14.1=${total_414}s, Z3 4.11.2=${total_411}s ($NUM_RUNS runs each)"
        log_progress "  -> mean total: Z3 4.14.1=${total_414}s, Z3 4.11.2=${total_411}s"
    else
        record_result "Table4/$paper_name" "FAIL" "One or more runs timed out"
    fi
done <<< "$TABLE4_BENCHMARKS"

echo ""

# ==================================================================
# Final Summary
# ==================================================================
echo ""
echo "============================================================"
echo "  SUMMARY"
echo "============================================================"
echo ""
printf "$SUMMARY"
echo ""
echo "  Passed: $PASS_COUNT"
echo "  Failed: $FAIL_COUNT"
echo ""
echo "  These results correspond to subsets of Tables 1, 3, and 4 in the paper."
echo ""
echo "  Results saved to: $SHORT_DIR/"
echo "    - table1_subset.csv"
echo "    - table3_subset.csv"
echo "    - table4_subset.csv"
echo "    - table4_raw/"
echo "============================================================"

if [ "$FAIL_COUNT" -gt 0 ]; then
    echo ""
    echo "WARNING: $FAIL_COUNT benchmark(s) failed. Check output above for details."
    exit 1
fi
