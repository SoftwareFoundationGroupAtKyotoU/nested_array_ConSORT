#!/usr/bin/env bash
# table4.sh — Reproduce Table 4: Mean verification time by Z3 version (10 runs)
set -euo pipefail

EVAL_DIR="$(cd "$(dirname "$0")" && pwd)"
source "$EVAL_DIR/lib/common.sh"

NUM_RUNS="${NUM_RUNS:-10}"

echo "============================================================"
echo "  Table 4: Mean Verification Time by Z3 Version"
echo "  ($NUM_RUNS runs per benchmark per Z3 version)"
echo "============================================================"

RAW_DIR="$RESULTS_DIR/table4_raw"
CSV_FILE="$RESULTS_DIR/table4.csv"
MD_FILE="$RESULTS_DIR/table4.md"

# ---- Build benchmark list as a temp file ----
BENCH_LIST=$(mktemp)
trap "rm -f $BENCH_LIST" EXIT

# Format: category|paper_name|imp_file|tool
while IFS= read -r paper_name; do
    file_base=$(get_nested_array_file "$paper_name")
    echo "Nest.|${paper_name}|./example/nested_arrays/${file_base}.imp|ours" >> "$BENCH_LIST"
done < <(read_nested_array_order)

while IFS= read -r paper_name; do
    file_base=$(get_int_array_file "$paper_name")
    echo "Int(O)|${paper_name}|./example/int_arrays/${file_base}.imp|ours" >> "$BENCH_LIST"
done < <(read_int_array_order)

while IFS= read -r paper_name; do
    file_base=$(get_int_array_file "$paper_name")
    echo "Int(T)|${paper_name}|examples/non-alias/${file_base}.imp|tanaka" >> "$BENCH_LIST"
done < <(read_int_array_order)

total_benchmarks=$(wc -l < "$BENCH_LIST" | tr -d ' ')
total_runs=$((total_benchmarks * 2 * NUM_RUNS))
completed=0

echo "Total benchmarks: $total_benchmarks"
echo "Total runs: $total_runs (estimated time: $((total_runs * 5 / 60))-$((total_runs * 30 / 60)) minutes)"
echo ""

# ---- Execute all runs (with resumability) ----
while IFS='|' read -r cat paper_name imp_file tool; do
    dir_name=$(echo "$paper_name" | tr '()' '__' | tr ' ' '_')

    for z3_ver in 4.14.1 4.11.2; do
        run_dir="$RAW_DIR/${cat}/${dir_name}/z3-${z3_ver}"
        mkdir -p "$run_dir"

        for run_num in $(seq 1 "$NUM_RUNS"); do
            completed=$((completed + 1))
            output_file="$run_dir/run_${run_num}.txt"

            if [ -f "$output_file" ]; then
                continue
            fi

            log_progress "[$completed/$total_runs] $cat/$paper_name (Z3 $z3_ver, run $run_num)"

            use_z3 "$z3_ver"

            if [ "$tool" = "ours" ]; then
                output=$(run_tool "$imp_file")
                own=$(parse_own_time "$output")
                ref=$(parse_ref_time "$output")
                total=$(parse_total_time "$output")
                echo "own=${own:-TIMEOUT} ref=${ref:-TIMEOUT} total=${total:-TIMEOUT}" > "$output_file"
            else
                output=$(run_tanaka "$imp_file" 2>&1) || true
                own=$(echo "$output" | awk '{print $1}')
                ref=$(echo "$output" | awk '{print $2}')
                total=$(echo "$output" | awk '{print $3}')
                echo "own=${own:-TIMEOUT} ref=${ref:-TIMEOUT} total=${total:-TIMEOUT}" > "$output_file"
            fi
        done
    done
done < "$BENCH_LIST"

echo ""
echo "--- All runs complete. Computing means... ---"

# ---- Compute means and generate table using Python ----
python3 - "$BENCH_LIST" "$RAW_DIR" "$NUM_RUNS" "$CSV_FILE" "$MD_FILE" << 'PYTHON_SCRIPT'
import sys
import os
import re

bench_list_file = sys.argv[1]
raw_dir = sys.argv[2]
num_runs = int(sys.argv[3])
csv_file = sys.argv[4]
md_file = sys.argv[5]

def sanitize_dir(name):
    return name.replace('(', '_').replace(')', '_').replace(' ', '_')

def parse_run_file(path, field):
    """Extract a field value from a run file like 'own=1.234 ref=0.567 total=1.801'"""
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

def compute_direction(v414, v411):
    if v414 is None or v411 is None:
        return "T/O"
    if v414 == 0 and v411 == 0:
        return "~"
    if v414 == 0:
        return "up"
    ratio = v411 / v414
    if ratio >= 1.05:
        return f"{ratio:.1f}x up"
    elif ratio <= 1 / 1.05:
        return f"{1/ratio:.1f}x down"
    else:
        return "~"

def fmt_val(v):
    if v is None:
        return "T/O"
    return f"{v:.3f}"

# Read benchmark list
benchmarks = []
with open(bench_list_file) as f:
    for line in f:
        line = line.strip()
        if not line:
            continue
        parts = line.split('|')
        benchmarks.append({
            "cat": parts[0],
            "paper_name": parts[1],
            "imp_file": parts[2],
            "tool": parts[3]
        })

# Compute results
results = []
for b in benchmarks:
    dir_name = sanitize_dir(b["paper_name"])
    dir_414 = os.path.join(raw_dir, b["cat"], dir_name, "z3-4.14.1")
    dir_411 = os.path.join(raw_dir, b["cat"], dir_name, "z3-4.11.2")

    own_414 = compute_mean(dir_414, "own", num_runs)
    own_411 = compute_mean(dir_411, "own", num_runs)
    ref_414 = compute_mean(dir_414, "ref", num_runs)
    ref_411 = compute_mean(dir_411, "ref", num_runs)
    total_414 = compute_mean(dir_414, "total", num_runs)
    total_411 = compute_mean(dir_411, "total", num_runs)

    results.append({
        "cat": b["cat"],
        "name": b["paper_name"],
        "own_414": own_414, "own_411": own_411,
        "ref_414": ref_414, "ref_411": ref_411,
        "total_414": total_414, "total_411": total_411,
        "own_dir": compute_direction(own_414, own_411),
        "ref_dir": compute_direction(ref_414, ref_411),
        "total_dir": compute_direction(total_414, total_411),
    })

# Write CSV
with open(csv_file, 'w') as f:
    f.write("Cat,Benchmark,Own_4.14,Own_4.11,Own_Dir,Ref_4.14,Ref_4.11,Ref_Dir,Total_4.14,Total_4.11,Total_Dir\n")
    for r in results:
        f.write(f"{r['cat']},{r['name']},{fmt_val(r['own_414'])},{fmt_val(r['own_411'])},{r['own_dir']},"
                f"{fmt_val(r['ref_414'])},{fmt_val(r['ref_411'])},{r['ref_dir']},"
                f"{fmt_val(r['total_414'])},{fmt_val(r['total_411'])},{r['total_dir']}\n")

# Write Markdown
with open(md_file, 'w') as f:
    f.write("# Table 4: Mean Verification Time by Z3 Version\n\n")
    f.write(f"Mean over {num_runs} runs. Direction: up = Z3 4.14.1 faster, down = Z3 4.11.2 faster, ~ = ratio < 1.05x\n\n")
    f.write("| Cat. | Benchmark | Own 4.14 | Own 4.11 | Dir | Ref 4.14 | Ref 4.11 | Dir | Total 4.14 | Total 4.11 | Dir |\n")
    f.write("|------|-----------|----------|----------|-----|----------|----------|-----|------------|------------|-----|\n")
    for r in results:
        f.write(f"| {r['cat']:<5s} | {r['name']:<22s} "
                f"| {fmt_val(r['own_414']):>8s} | {fmt_val(r['own_411']):>8s} | {r['own_dir']:<9s} "
                f"| {fmt_val(r['ref_414']):>8s} | {fmt_val(r['ref_411']):>8s} | {r['ref_dir']:<9s} "
                f"| {fmt_val(r['total_414']):>10s} | {fmt_val(r['total_411']):>10s} | {r['total_dir']:<9s} |\n")

print("Table 4 generated successfully.")
PYTHON_SCRIPT

echo ""
echo "============================================================"
echo "  Table 4 complete"
echo "  CSV: $CSV_FILE"
echo "  Markdown: $MD_FILE"
echo "  Raw data: $RAW_DIR/"
echo "============================================================"
echo ""
cat "$MD_FILE"
