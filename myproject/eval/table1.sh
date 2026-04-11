#!/usr/bin/env bash
# table1.sh — Reproduce Table 1: Nested array benchmark results
set -euo pipefail

EVAL_DIR="$(cd "$(dirname "$0")" && pwd)"
source "$EVAL_DIR/lib/common.sh"

# Use Z3 4.14.1 (the version used in the paper)
use_z3 "4.14.1"

echo "============================================================"
echo "  Table 1: Nested Array Benchmark Results"
echo "============================================================"

CSV_FILE="$RESULTS_DIR/table1.csv"
MD_FILE="$RESULTS_DIR/table1.md"
RAW_DIR="$RESULTS_DIR/raw/table1"
mkdir -p "$RAW_DIR"

# CSV header
echo "Name,Total,Ownership,Refinement,Manual_Alias,Auto_Alias,Total_Alias" > "$CSV_FILE"

# Markdown header
{
    echo "# Table 1: Nested Array Benchmark Results"
    echo ""
    echo "| Name | Total time (ownership+refinement) | # Alias Manual | # Alias Auto | # Alias Total |"
    echo "|------|-----------------------------------|----------------|--------------|---------------|"
} > "$MD_FILE"

total_benchmarks=$(read_nested_array_order | wc -l | tr -d ' ')
current=0

while IFS= read -r paper_name; do
    current=$((current + 1))
    file_base=$(get_nested_array_file "$paper_name")
    imp_file="./example/nested_arrays/${file_base}.imp"
    omit_base=$(get_omit_alias_name "$file_base")
    omit_file="./example/omit_alias/${omit_base}.imp"

    log_progress "[$current/$total_benchmarks] Running $paper_name ($file_base.imp)..."

    # Run verification
    output=$(run_tool "$imp_file")
    echo "$output" > "$RAW_DIR/${file_base}.txt"

    own_time=$(parse_own_time "$output")
    ref_time=$(parse_ref_time "$output")
    total_time=$(parse_total_time "$output")

    if [ -z "$total_time" ]; then
        # Diagnose the cause of failure
        error_cause="unknown"
        if echo "$output" | grep -q "program error"; then
            error_cause="ownership unsat (Z3 returned unsat on ownership constraints)"
        elif echo "$output" | grep -q "TIME LIMIT" || echo "$output" | grep -q "timeout"; then
            error_cause="timeout (exceeded ${TIMEOUT_SEC}s)"
        elif echo "$output" | grep -q "refinement: unsat"; then
            error_cause="refinement unsat (hoice returned unsat on CHC constraints)"
        elif echo "$output" | grep -qi "error"; then
            error_cause=$(echo "$output" | grep -i "error" | head -1)
        elif [ -z "$output" ]; then
            error_cause="no output (tool may have crashed or timed out)"
        else
            # Check if ownership succeeded but refinement failed to produce timing
            own_result=$(parse_ownership_result "$output")
            if [ "$own_result" = "sat" ]; then
                error_cause="refinement phase failed or timed out (ownership succeeded)"
            else
                error_cause="failed to parse output"
            fi
        fi
        echo "  WARNING: Failed for $paper_name: $error_cause"
        own_time="ERROR"
        ref_time="ERROR"
        total_time="ERROR"
    fi

    # Count aliases
    # Manual: aliases in omit_alias version WITHOUT -insert_alias
    manual_alias=$(count_aliases "$omit_file")

    # Total with auto-insertion on omit_alias version
    total_with_insert=$(count_aliases "$omit_file" "-insert_alias")
    auto_alias=$((total_with_insert - manual_alias))

    total_alias=$((manual_alias + auto_alias))

    # CSV
    echo "$paper_name,$total_time,$own_time,$ref_time,$manual_alias,$auto_alias,$total_alias" >> "$CSV_FILE"

    # Markdown
    if [ "$total_time" != "ERROR" ]; then
        printf "| %-22s | %s (%s+%s) | %d | %d | %d |\n" \
            "$paper_name" "$total_time" "$own_time" "$ref_time" \
            "$manual_alias" "$auto_alias" "$total_alias" >> "$MD_FILE"
    else
        printf "| %-22s | ERROR | - | - | - |\n" "$paper_name" >> "$MD_FILE"
    fi

    log_progress "  -> total=${total_time}s (own=${own_time}s + ref=${ref_time}s), alias: manual=$manual_alias auto=$auto_alias total=$total_alias"
done < <(read_nested_array_order)

echo ""
echo "============================================================"
echo "  Table 1 complete"
echo "  CSV: $CSV_FILE"
echo "  Markdown: $MD_FILE"
echo "  Raw output: $RAW_DIR/"
echo "============================================================"
echo ""
cat "$MD_FILE"
