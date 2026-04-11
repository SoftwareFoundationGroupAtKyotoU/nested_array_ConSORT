#!/usr/bin/env bash
# run_all.sh — Master script: reproduce all tables from the paper
set -euo pipefail

EVAL_DIR="$(cd "$(dirname "$0")" && pwd)"

echo "============================================================"
echo "  Artifact Evaluation: Reproduce All Tables"
echo "============================================================"
echo ""

# ---- Step 1: Setup ----
# Skip setup if --no-setup is passed (e.g., inside Docker where deps are pre-installed)
if [ "${1:-}" = "--no-setup" ]; then
    echo "=== Step 1/6: Setup (skipped: --no-setup) ==="
    source "$EVAL_DIR/lib/common.sh"
else
    echo "=== Step 1/6: Setup ==="
    bash "$EVAL_DIR/setup.sh"
fi

# ---- Step 2: Table 1 ----
echo ""
echo "=== Step 2/6: Table 1 (Nested Array Benchmarks) ==="
bash "$EVAL_DIR/table1.sh"

# ---- Step 3: Table 2 ----
echo ""
echo "=== Step 3/6: Table 2 (Ownership Terms) ==="
bash "$EVAL_DIR/table2.sh"

# ---- Step 4: Table 3 ----
echo ""
echo "=== Step 4/6: Table 3 (Comparison with Tanaka et al.) ==="
bash "$EVAL_DIR/table3.sh"

# ---- Step 5: Table 4 ----
echo ""
echo "=== Step 5/6: Table 4 (Mean Verification Time, 10 runs x 2 Z3 versions) ==="
echo "WARNING: This step runs 700 verification tasks and may take 1-3 hours."
echo "Set NUM_RUNS=2 for a quick check: NUM_RUNS=2 bash $EVAL_DIR/table4.sh"
echo ""
bash "$EVAL_DIR/table4.sh"

# ---- Step 6: Table 5 ----
echo ""
echo "=== Step 6/6: Table 5 (Z3 Version Comparison Summary) ==="
bash "$EVAL_DIR/table5.sh"

echo ""
echo "============================================================"
echo "  All tables generated!"
echo ""
echo "  Results:"
echo "    Table 1: $EVAL_DIR/results/table1.md"
echo "    Table 2: $EVAL_DIR/results/table2.md"
echo "    Table 3: $EVAL_DIR/results/table3.md"
echo "    Table 4: $EVAL_DIR/results/table4.md"
echo "    Table 5: $EVAL_DIR/results/table5.md"
echo ""
echo "  CSV files also available in $EVAL_DIR/results/"
echo "============================================================"
