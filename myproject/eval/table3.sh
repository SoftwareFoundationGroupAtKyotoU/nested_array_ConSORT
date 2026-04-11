#!/usr/bin/env bash
# table3.sh — Reproduce Table 3: Comparison with Tanaka et al.
set -euo pipefail

EVAL_DIR="$(cd "$(dirname "$0")" && pwd)"
source "$EVAL_DIR/lib/common.sh"

echo "============================================================"
echo "  Table 3: Comparison with Tanaka et al."
echo "============================================================"

CSV_FILE="$RESULTS_DIR/table3.csv"
MD_FILE="$RESULTS_DIR/table3.md"
RAW_DIR="$RESULTS_DIR/raw/table3"
mkdir -p "$RAW_DIR"

# CSV header
echo "Name,Tanaka_4.11.2_Total,Tanaka_4.11.2_Own,Tanaka_4.11.2_Ref,Ours_4.11.2_Total,Ours_4.11.2_Own,Ours_4.11.2_Ref,Ours_4.14.1_Total,Ours_4.14.1_Own,Ours_4.14.1_Ref" > "$CSV_FILE"

# Markdown header
{
    echo "# Table 3: Comparison with Tanaka et al."
    echo ""
    echo "| Name | Tanaka+ (Z3 4.11.2) | Ours (Z3 4.11.2) | Ours (Z3 4.14.1) |"
    echo "|------|---------------------|-------------------|-------------------|"
} > "$MD_FILE"

total=$(read_int_array_order | wc -l | tr -d ' ')
current=0

while IFS= read -r paper_name; do
    current=$((current + 1))
    file_base=$(get_int_array_file "$paper_name")

    log_progress "[$current/$total] $paper_name ($file_base.imp)"

    # ---- Tanaka+ (Z3 4.11.2) ----
    log_progress "  Running Tanaka+ (Z3 4.11.2)..."
    use_z3 "4.11.2"

    tanaka_output=$(run_tanaka "examples/non-alias/${file_base}.imp" 2>&1) || true
    echo "$tanaka_output" > "$RAW_DIR/${file_base}_tanaka_4.11.2.txt"

    tanaka_own=$(echo "$tanaka_output" | awk '{print $1}')
    tanaka_ref=$(echo "$tanaka_output" | awk '{print $2}')
    tanaka_total=$(echo "$tanaka_output" | awk '{print $3}')

    # ---- Ours (Z3 4.11.2) ----
    log_progress "  Running Ours (Z3 4.11.2)..."
    output_411=$(run_tool "./example/int_arrays/${file_base}.imp")
    echo "$output_411" > "$RAW_DIR/${file_base}_ours_4.11.2.txt"

    ours_411_own=$(parse_own_time "$output_411")
    ours_411_ref=$(parse_ref_time "$output_411")
    ours_411_total=$(parse_total_time "$output_411")

    # ---- Ours (Z3 4.14.1) ----
    log_progress "  Running Ours (Z3 4.14.1)..."
    use_z3 "4.14.1"
    output_414=$(run_tool "./example/int_arrays/${file_base}.imp")
    echo "$output_414" > "$RAW_DIR/${file_base}_ours_4.14.1.txt"

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

    # CSV
    echo "$paper_name,$tanaka_total,$tanaka_own,$tanaka_ref,$ours_411_total,$ours_411_own,$ours_411_ref,$ours_414_total,$ours_414_own,$ours_414_ref" >> "$CSV_FILE"

    # Markdown
    printf "| %-12s | %s (%s+%s) | %s (%s+%s) | %s (%s+%s) |\n" \
        "$paper_name" \
        "$tanaka_total" "$tanaka_own" "$tanaka_ref" \
        "$ours_411_total" "$ours_411_own" "$ours_411_ref" \
        "$ours_414_total" "$ours_414_own" "$ours_414_ref" >> "$MD_FILE"

    log_progress "  -> Tanaka+: ${tanaka_total}s, Ours(4.11.2): ${ours_411_total}s, Ours(4.14.1): ${ours_414_total}s"
done < <(read_int_array_order)

echo ""
echo "============================================================"
echo "  Table 3 complete"
echo "  CSV: $CSV_FILE"
echo "  Markdown: $MD_FILE"
echo "============================================================"
echo ""
cat "$MD_FILE"
