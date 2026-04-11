#!/usr/bin/env bash
# table5.sh — Reproduce Table 5: Z3 version comparison summary
#
# Reads table4.csv and summarizes: for each category, how many benchmarks
# Z3 4.14.1 is faster vs Z3 4.11.2 (for ownership and refinement phases).
# Threshold: >=1.05x ratio to count as "faster"; otherwise "comparable".
set -euo pipefail

EVAL_DIR="$(cd "$(dirname "$0")" && pwd)"
source "$EVAL_DIR/lib/common.sh"

CSV_INPUT="$RESULTS_DIR/table4.csv"
CSV_FILE="$RESULTS_DIR/table5.csv"
MD_FILE="$RESULTS_DIR/table5.md"

if [ ! -f "$CSV_INPUT" ]; then
    echo "ERROR: $CSV_INPUT not found. Run table4.sh first." >&2
    exit 1
fi

echo "============================================================"
echo "  Table 5: Z3 Version Comparison Summary"
echo "============================================================"

python3 - "$CSV_INPUT" "$CSV_FILE" "$MD_FILE" << 'PYEOF'
import csv
import sys
from collections import defaultdict

THRESHOLD = 1.05

input_file = sys.argv[1]
csv_out = sys.argv[2]
md_out = sys.argv[3]

# Read table4.csv
# Columns: Category,Name,Z3_4.14.1_Own,Z3_4.14.1_Ref,Z3_4.14.1_Total,Z3_4.11.2_Own,Z3_4.11.2_Ref,Z3_4.11.2_Total
rows = []
with open(input_file) as f:
    reader = csv.DictReader(f)
    for row in reader:
        rows.append(row)

# Per-category summary
categories = defaultdict(lambda: {
    "own_414_faster": 0, "own_411_faster": 0, "own_comparable": 0,
    "ref_414_faster": 0, "ref_411_faster": 0, "ref_comparable": 0,
    "own_reversed": 0, "total": 0,
})

for row in rows:
    cat = row["Category"]
    stats = categories[cat]
    stats["total"] += 1

    try:
        own_414 = float(row["Z3_4.14.1_Own"])
        own_411 = float(row["Z3_4.11.2_Own"])
        ref_414 = float(row["Z3_4.14.1_Ref"])
        ref_411 = float(row["Z3_4.11.2_Ref"])
    except (ValueError, KeyError):
        continue

    # Ownership phase comparison
    if own_414 > 0 and own_411 > 0:
        ratio_own = own_411 / own_414
        if ratio_own >= THRESHOLD:
            stats["own_414_faster"] += 1
            own_winner = "4.14.1"
        elif 1.0 / ratio_own >= THRESHOLD:
            stats["own_411_faster"] += 1
            own_winner = "4.11.2"
        else:
            stats["own_comparable"] += 1
            own_winner = "comparable"
    else:
        own_winner = "unknown"

    # Refinement phase comparison
    if ref_414 > 0 and ref_411 > 0:
        ratio_ref = ref_411 / ref_414
        if ratio_ref >= THRESHOLD:
            stats["ref_414_faster"] += 1
            ref_winner = "4.14.1"
        elif 1.0 / ratio_ref >= THRESHOLD:
            stats["ref_411_faster"] += 1
            ref_winner = "4.11.2"
        else:
            stats["ref_comparable"] += 1
            ref_winner = "comparable"
    else:
        ref_winner = "unknown"

    # Reversed: faster version differs between phases
    if own_winner in ("4.14.1", "4.11.2") and ref_winner in ("4.14.1", "4.11.2"):
        if own_winner != ref_winner:
            stats["own_reversed"] += 1

# Write CSV
with open(csv_out, "w") as f:
    writer = csv.writer(f)
    writer.writerow(["Category", "Total",
                     "Own_4.14.1_Faster", "Own_4.11.2_Faster", "Own_Comparable",
                     "Ref_4.14.1_Faster", "Ref_4.11.2_Faster", "Ref_Comparable",
                     "Reversed"])
    for cat in sorted(categories.keys()):
        s = categories[cat]
        writer.writerow([cat, s["total"],
                         s["own_414_faster"], s["own_411_faster"], s["own_comparable"],
                         s["ref_414_faster"], s["ref_411_faster"], s["ref_comparable"],
                         s["own_reversed"]])

# Write Markdown
with open(md_out, "w") as f:
    f.write("# Table 5: Z3 Version Comparison Summary\n\n")
    f.write(f"Threshold: {THRESHOLD}x (ratios below this are counted as comparable)\n\n")
    f.write("| Category | # Benchmarks | Own: 4.14.1 faster | Own: 4.11.2 faster | Own: comparable | Ref: 4.14.1 faster | Ref: 4.11.2 faster | Ref: comparable | Reversed |\n")
    f.write("|----------|--------------|--------------------|--------------------|-----------------|--------------------|--------------------|-----------------|----------|\n")
    for cat in sorted(categories.keys()):
        s = categories[cat]
        f.write(f"| {cat:<8} | {s['total']:>12} | {s['own_414_faster']:>18} | {s['own_411_faster']:>18} | {s['own_comparable']:>15} | {s['ref_414_faster']:>18} | {s['ref_411_faster']:>18} | {s['ref_comparable']:>15} | {s['own_reversed']:>8} |\n")

print(f"Summary written to {md_out}")
PYEOF

echo ""
echo "============================================================"
echo "  Table 5 complete"
echo "  CSV: $CSV_FILE"
echo "  Markdown: $MD_FILE"
echo "============================================================"
echo ""
cat "$MD_FILE"
