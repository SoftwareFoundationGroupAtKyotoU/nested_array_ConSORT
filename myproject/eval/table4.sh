#!/usr/bin/env bash
# table4.sh — Reproduce Table 4: Z3 version comparison (single run per benchmark)
#
# Runs all 35 benchmarks (19 nested array + 8 int_ours + 8 int_tanaka)
# with both Z3 4.14.1 and Z3 4.11.2.
#
# The paper uses 10 runs and reports the mean. This script uses a single run
# to keep evaluation time manageable. Results may show more variance than
# the paper's averaged values.
set -euo pipefail

EVAL_DIR="$(cd "$(dirname "$0")" && pwd)"
source "$EVAL_DIR/lib/common.sh"

echo "============================================================"
echo "  Table 4: Z3 Version Comparison (single run)"
echo "============================================================"
echo ""
echo "  NOTE: The paper reports mean times over 10 runs."
echo "  This script performs a single run per benchmark."
echo ""

CSV_FILE="$RESULTS_DIR/table4.csv"
MD_FILE="$RESULTS_DIR/table4.md"
RAW_DIR="$RESULTS_DIR/raw/table4"
mkdir -p "$RAW_DIR"

# CSV header
echo "Category,Name,Z3_4.14.1_Own,Z3_4.14.1_Ref,Z3_4.14.1_Total,Z3_4.11.2_Own,Z3_4.11.2_Ref,Z3_4.11.2_Total" > "$CSV_FILE"

# Markdown header
{
    echo "# Table 4: Z3 Version Comparison"
    echo ""
    echo "Single run per benchmark. The paper reports mean times over 10 runs."
    echo ""
    echo "| Category | Name | Z3 4.14.1 (own+ref=total) | Z3 4.11.2 (own+ref=total) |"
    echo "|----------|------|---------------------------|---------------------------|"
} > "$MD_FILE"

# ---- Helper: run one benchmark with both Z3 versions ----
run_both_z3() {
    local category="$1"
    local paper_name="$2"
    local imp_file="$3"
    local run_func="$4"  # "run_tool" or "run_tanaka"

    # Z3 4.14.1
    use_z3 "4.14.1"
    local output_414
    if [ "$run_func" = "run_tanaka" ]; then
        output_414=$(run_tanaka "$imp_file" 2>&1) || true
        echo "$output_414" > "$RAW_DIR/${category}_$(echo "$paper_name" | tr ' ' '_')_4.14.1.txt"
        local own_414 ref_414 total_414
        own_414=$(echo "$output_414" | awk '{print $1}')
        ref_414=$(echo "$output_414" | awk '{print $2}')
        total_414=$(echo "$output_414" | awk '{print $3}')
    else
        output_414=$(run_tool "$imp_file")
        echo "$output_414" > "$RAW_DIR/${category}_$(echo "$paper_name" | tr ' ' '_')_4.14.1.txt"
        local own_414 ref_414 total_414
        own_414=$(parse_own_time "$output_414")
        ref_414=$(parse_ref_time "$output_414")
        total_414=$(parse_total_time "$output_414")
    fi

    # Z3 4.11.2
    use_z3 "4.11.2"
    local output_411
    if [ "$run_func" = "run_tanaka" ]; then
        output_411=$(run_tanaka "$imp_file" 2>&1) || true
        echo "$output_411" > "$RAW_DIR/${category}_$(echo "$paper_name" | tr ' ' '_')_4.11.2.txt"
        local own_411 ref_411 total_411
        own_411=$(echo "$output_411" | awk '{print $1}')
        ref_411=$(echo "$output_411" | awk '{print $2}')
        total_411=$(echo "$output_411" | awk '{print $3}')
    else
        output_411=$(run_tool "$imp_file")
        echo "$output_411" > "$RAW_DIR/${category}_$(echo "$paper_name" | tr ' ' '_')_4.11.2.txt"
        local own_411 ref_411 total_411
        own_411=$(parse_own_time "$output_411")
        ref_411=$(parse_ref_time "$output_411")
        total_411=$(parse_total_time "$output_411")
    fi

    # Defaults
    own_414="${own_414:-TIMEOUT}"
    ref_414="${ref_414:-TIMEOUT}"
    total_414="${total_414:-TIMEOUT}"
    own_411="${own_411:-TIMEOUT}"
    ref_411="${ref_411:-TIMEOUT}"
    total_411="${total_411:-TIMEOUT}"

    # CSV
    echo "$category,$paper_name,$own_414,$ref_414,$total_414,$own_411,$ref_411,$total_411" >> "$CSV_FILE"

    # Markdown
    printf "| %-8s | %-22s | %s+%s=%s | %s+%s=%s |\n" \
        "$category" "$paper_name" \
        "$own_414" "$ref_414" "$total_414" \
        "$own_411" "$ref_411" "$total_411" >> "$MD_FILE"

    log_progress "  -> 4.14.1: ${total_414}s, 4.11.2: ${total_411}s"
}

# ---- Category 1: Nested array benchmarks (19) ----
echo "--- Nested Array Benchmarks ---"
nest_total=$(read_nested_array_order | wc -l | tr -d ' ')
nest_current=0

while IFS= read -r paper_name; do
    nest_current=$((nest_current + 1))
    file_base=$(get_nested_array_file "$paper_name")
    imp_file="./example/nested_arrays/${file_base}.imp"

    log_progress "[$nest_current/$nest_total] $paper_name ($file_base.imp)"
    run_both_z3 "nest" "$paper_name" "$imp_file" "run_tool"
done < <(read_nested_array_order)

# ---- Category 2: Integer array benchmarks — Ours (8) ----
echo ""
echo "--- Integer Array Benchmarks (Ours) ---"
int_total=$(read_int_array_order | wc -l | tr -d ' ')
int_current=0

while IFS= read -r paper_name; do
    int_current=$((int_current + 1))
    file_base=$(get_int_array_file "$paper_name")
    imp_file="./example/int_arrays/${file_base}.imp"

    log_progress "[$int_current/$int_total] $paper_name ($file_base.imp)"
    run_both_z3 "int_ours" "$paper_name" "$imp_file" "run_tool"
done < <(read_int_array_order)

# ---- Category 3: Integer array benchmarks — Tanaka+ (8) ----
echo ""
echo "--- Integer Array Benchmarks (Tanaka+) ---"
int_current=0

if [ -d "$EXTENDED_CONSORT_DIR/src" ]; then
    while IFS= read -r paper_name; do
        int_current=$((int_current + 1))
        file_base=$(get_int_array_file "$paper_name")
        imp_file="examples/non-alias/${file_base}.imp"

        log_progress "[$int_current/$int_total] $paper_name ($file_base.imp)"
        run_both_z3 "int_tanaka" "$paper_name" "$imp_file" "run_tanaka"
    done < <(read_int_array_order)
else
    echo "  WARNING: Extended_ConSORT not found. Skipping Tanaka+ benchmarks."
    echo "  Run setup.sh to install Extended_ConSORT."
fi

echo ""
echo "============================================================"
echo "  Table 4 complete"
echo "  CSV: $CSV_FILE"
echo "  Markdown: $MD_FILE"
echo "  Raw output: $RAW_DIR/"
echo "============================================================"
echo ""
cat "$MD_FILE"
