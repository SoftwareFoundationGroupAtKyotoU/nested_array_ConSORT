#!/usr/bin/env bash
# table5.sh — Reproduce Table 5: Z3 version comparison summary (derived from Table 4)
set -euo pipefail

EVAL_DIR="$(cd "$(dirname "$0")" && pwd)"
source "$EVAL_DIR/lib/common.sh"

echo "============================================================"
echo "  Table 5: Z3 Version Comparison Summary"
echo "============================================================"

CSV_INPUT="$RESULTS_DIR/table4.csv"
CSV_FILE="$RESULTS_DIR/table5.csv"
MD_FILE="$RESULTS_DIR/table5.md"

if [ ! -f "$CSV_INPUT" ]; then
    echo "ERROR: $CSV_INPUT not found. Run table4.sh first."
    exit 1
fi

python3 - "$CSV_INPUT" "$CSV_FILE" "$MD_FILE" << 'PYTHON_SCRIPT'
import csv
import sys

input_file = sys.argv[1]
csv_output = sys.argv[2]
md_output = sys.argv[3]

# Read Table 4 CSV
# Columns: Cat,Benchmark,Own_4.14,Own_4.11,Own_Dir,Ref_4.14,Ref_4.11,Ref_Dir,Total_4.14,Total_4.11,Total_Dir
rows = []
with open(input_file, 'r') as f:
    reader = csv.DictReader(f)
    for row in reader:
        rows.append(row)

categories = ["Nest.", "Int(O)", "Int(T)"]
cat_labels = {
    "Nest.": "Nested Array",
    "Int(O)": "Int Array (Ours)",
    "Int(T)": "Int Array (Tanaka et al.)"
}

def classify_direction(v414_str, v411_str):
    """Returns 'up' (4.14 faster), 'down' (4.11 faster), 'approx', or 'timeout'"""
    if v414_str == "TIMEOUT" or v411_str == "TIMEOUT":
        return "timeout"
    try:
        v414 = float(v414_str)
        v411 = float(v411_str)
    except ValueError:
        return "timeout"
    if v414 == 0 and v411 == 0:
        return "approx"
    if v414 == 0:
        return "up"
    ratio = v411 / v414
    if ratio >= 1.05:
        return "up"  # 4.14 is faster
    elif ratio <= 1.0 / 1.05:
        return "down"  # 4.11 is faster
    else:
        return "approx"

results = {}
for cat in categories:
    cat_rows = [r for r in rows if r["Cat"] == cat]

    own_up = own_down = own_approx = 0
    ref_up = ref_down = ref_approx = 0
    rev_count = 0
    excluded = 0

    for r in cat_rows:
        own_dir = classify_direction(r["Own_4.14"], r["Own_4.11"])
        ref_dir = classify_direction(r["Ref_4.14"], r["Ref_4.11"])

        # Exclude timeouts from numeric counts
        if own_dir == "timeout" or ref_dir == "timeout":
            excluded += 1
            continue

        if own_dir == "up": own_up += 1
        elif own_dir == "down": own_down += 1
        else: own_approx += 1

        if ref_dir == "up": ref_up += 1
        elif ref_dir == "down": ref_down += 1
        else: ref_approx += 1

        # Reversal: faster version differs between own and ref (both >= 1.05x)
        if own_dir in ("up", "down") and ref_dir in ("up", "down") and own_dir != ref_dir:
            rev_count += 1

    results[cat] = {
        "own_up": own_up, "own_down": own_down, "own_approx": own_approx,
        "ref_up": ref_up, "ref_down": ref_down, "ref_approx": ref_approx,
        "rev": rev_count, "excluded": excluded
    }

# Write CSV
with open(csv_output, 'w') as f:
    f.write("Category,Own_4.14_faster,Own_4.11_faster,Own_Approx,Ref_4.14_faster,Ref_4.11_faster,Ref_Approx,Reversals\n")
    for cat in categories:
        r = results[cat]
        f.write(f"{cat},{r['own_up']},{r['own_down']},{r['own_approx']},{r['ref_up']},{r['ref_down']},{r['ref_approx']},{r['rev']}\n")

# Write Markdown
with open(md_output, 'w') as f:
    f.write("# Table 5: Z3 Version Comparison Summary\n\n")
    f.write("Number of benchmarks where each Z3 version is faster (>= 1.05x).\n")
    f.write('"~" counts benchmarks with ratio < 1.05x. "Rev." counts benchmarks where the faster\n')
    f.write("version differs between ownership and refinement. Benchmarks with timeout excluded.\n\n")
    f.write("| Category | Ownership 4.14 | Ownership 4.11 | Own ~ | Refinement 4.14 | Refinement 4.11 | Ref ~ | Rev. |\n")
    f.write("|----------|----------------|----------------|-------|-----------------|-----------------|-------|------|\n")
    for cat in categories:
        r = results[cat]
        label = cat_labels[cat]
        f.write(f"| {label:<25s} | {r['own_up']:>14d} | {r['own_down']:>14d} | {r['own_approx']:>5d} | {r['ref_up']:>15d} | {r['ref_down']:>15d} | {r['ref_approx']:>5d} | {r['rev']:>4d} |\n")

print("Table 5 generated successfully.")
PYTHON_SCRIPT

echo ""
echo "============================================================"
echo "  Table 5 complete"
echo "  CSV: $CSV_FILE"
echo "  Markdown: $MD_FILE"
echo "============================================================"
echo ""
cat "$MD_FILE"
