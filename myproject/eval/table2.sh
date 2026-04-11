#!/usr/bin/env bash
# table2.sh — Reproduce Table 2: Inferred ownership terms
set -euo pipefail

EVAL_DIR="$(cd "$(dirname "$0")" && pwd)"
source "$EVAL_DIR/lib/common.sh"

echo "============================================================"
echo "  Table 2: Inferred Ownership Terms"
echo "============================================================"

TABLE2_DIR="$RESULTS_DIR/table2"
MD_FILE="$RESULTS_DIR/table2.md"
mkdir -p "$TABLE2_DIR"

# Representative benchmarks from Table 2 in the paper
TABLE2_BENCHMARKS="Indexed-Matrix(2D)
Sum-Matrix
Trans-Matrix
Share-Add-Matrix
Indexed-Matrix(3D)"

{
    echo "# Table 2: Inferred Ownership Terms"
    echo ""
    echo "This table shows inferred ownership terms for representative benchmarks."
    echo "The ownership solutions are extracted from the tool output after verification."
    echo ""
    echo "| Program | Ownership Solution File |"
    echo "|---------|------------------------|"
} > "$MD_FILE"

total=$(echo "$TABLE2_BENCHMARKS" | wc -l | tr -d ' ')
current=0

echo "$TABLE2_BENCHMARKS" | while IFS= read -r paper_name; do
    current=$((current + 1))
    file_base=$(get_nested_array_file "$paper_name")
    imp_file="./example/nested_arrays/${file_base}.imp"

    log_progress "[$current/$total] Running $paper_name ($file_base.imp)..."

    # Run verification (populates experiment/out_sat_ans.smt2)
    run_tool "$imp_file" > /dev/null

    # Copy ownership solution
    local_sat="$MYPROJECT_DIR/experiment/out_sat_ans.smt2"
    local_own="$MYPROJECT_DIR/experiment/own_result/result_${file_base}.imp"

    if [ -f "$local_sat" ]; then
        cp "$local_sat" "$TABLE2_DIR/${file_base}_sat_ans.smt2"
    fi
    if [ -f "$local_own" ]; then
        cp "$local_own" "$TABLE2_DIR/${file_base}_own_result.txt"
    fi

    echo "| $paper_name | [${file_base}_sat_ans.smt2](table2/${file_base}_sat_ans.smt2) |" >> "$MD_FILE"

    log_progress "  -> ownership solution saved"
done

{
    echo ""
    echo "Note: Ownership terms in the paper (Table 2) are manually interpreted from these solver outputs."
    echo "The r_outer and r_inner columns describe the ownership term templates inferred for the matrix pointer."
} >> "$MD_FILE"

echo ""
echo "============================================================"
echo "  Table 2 complete"
echo "  Ownership solutions: $TABLE2_DIR/"
echo "  Summary: $MD_FILE"
echo "============================================================"
echo ""
cat "$MD_FILE"
