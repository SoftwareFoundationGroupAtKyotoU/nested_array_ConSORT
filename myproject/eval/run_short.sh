#!/usr/bin/env bash
# run_short.sh — "Kick-the-tires" script for ECOOP artifact evaluation
# Runs a subset of benchmarks from Tables 1 and 3 in under 10 minutes.
set -euo pipefail

EVAL_DIR="$(cd "$(dirname "$0")" && pwd)"
source "$EVAL_DIR/lib/common.sh"

# Use Z3 4.14.1 as default
use_z3 "4.14.1"

echo "============================================================"
echo "  Kick-the-Tires: Quick Verification (subset of Tables 1 and 3)"
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
echo "  These results correspond to subsets of Tables 1 and 3 in the paper."
echo ""
echo "  Results saved to: $SHORT_DIR/"
echo "    - table1_subset.csv"
echo "    - table3_subset.csv"
echo "============================================================"

if [ "$FAIL_COUNT" -gt 0 ]; then
    echo ""
    echo "WARNING: $FAIL_COUNT benchmark(s) failed. Check output above for details."
    exit 1
fi
