# Artifact Documentation: Ownership Refinement Types for Pointer Arithmetic and Nested Arrays

ECOOP 2026 Artifact Evaluation

## 1. Overview

This artifact accompanies the paper "Ownership Refinement Types for Pointer Arithmetic and Nested Arrays". It provides `nested_array_ConSORT`, a tool for automated ownership type inference and refinement type checking for imperative programs with pointer arithmetic and nested arrays, extending [ConSORT](https://www.fos.kuis.kyoto-u.ac.jp/projects/consort/).

The artifact supports the following claims from the paper:

- **Table 1**: Verification timing and alias statement counts for 19 nested array benchmarks.
- **Table 2**: Inferred ownership term solutions (qualitative correctness).
- **Table 3**: Performance comparison with Tanaka et al. on 8 integer array benchmarks.
- **Table 4**: Mean verification time over 10 runs with two Z3 versions (4.11.2 and 4.14.1).
- **Table 5**: Summary of Z3 version comparison (derived from Table 4 data).

## 2. Requirements

- **Docker** (tested with Docker 20.x and later)
- Approximately **4 GB** of disk space for the Docker image
- **x86_64** Linux or macOS with Docker support
- No internet connection required after loading the image

## 3. Getting Started (Kick-the-Tires, 30 minutes or less)

### Step 1: Load the Docker image

```sh
docker load -i artifact.tar.gz
```

### Step 2: Start the container

```sh
docker run -it nested-array-consort:ecoop26
```

You will be placed in the `/home/opam/app/myproject/` directory inside the container.

### Step 3: Run the short evaluation

```sh
bash eval/run_short.sh
```

### Step 4: Verify the output

The short evaluation runs a small subset of benchmarks and should complete within 10 minutes. You should see:

- A few nested array benchmarks from Table 1 with timing output (ownership time, refinement time, total time).
- One integer array benchmark from Table 3 comparing our tool with Extended_ConSORT (Tanaka et al.).
- A brief Table 4 check with `NUM_RUNS=1` on a few benchmarks.
- Final summary indicating which paper tables correspond to each output.

If all benchmarks report `ownership: sat` and `refinement: sat`, the tool is working correctly.

## 4. Full Evaluation (2-4 hours)

### Run all tables at once

```sh
bash eval/run_all.sh
```

This script runs all five table reproduction scripts sequentially and stores results in `eval/results/`.

### Run individual tables

If you prefer to run tables independently:

```sh
bash eval/table1.sh    # Table 1: Nested array benchmarks (~10 min)
bash eval/table2.sh    # Table 2: Ownership term solutions (~5 min)
bash eval/table3.sh    # Table 3: Comparison with Tanaka et al. (~10 min)
bash eval/table4.sh    # Table 4: Mean time, 10 runs x 2 Z3 versions (~1-3 hours)
bash eval/table5.sh    # Table 5: Z3 version comparison summary (~1 min, requires Table 4 data)
```

### Speeding up Table 4

Table 4 is the most time-consuming step (700 verification tasks: 35 benchmarks x 10 runs x 2 Z3 versions). To reduce evaluation time:

```sh
NUM_RUNS=2 bash eval/table4.sh
```

This runs only 2 iterations instead of 10, completing in roughly 15-30 minutes. The resulting averages will be noisier but sufficient to confirm the general trend.

## 5. Claims and Evidence Mapping

| Paper Section | Script | Output File | What to Check |
|---|---|---|---|
| Table 1 | `eval/table1.sh` | `eval/results/table1.md` | Timing and alias counts for 19 nested array benchmarks |
| Table 2 | `eval/table2.sh` | `eval/results/table2/` | Ownership term solutions (SMT2 files; manual interpretation needed) |
| Table 3 | `eval/table3.sh` | `eval/results/table3.md` | Timing comparison with Tanaka et al. on 8 integer array benchmarks |
| Table 4 | `eval/table4.sh` | `eval/results/table4.md` | Mean verification time over 10 runs with Z3 4.11.2 and Z3 4.14.1 |
| Table 5 | `eval/table5.sh` | `eval/results/table5.md` | Z3 version comparison summary (derived from Table 4 data) |

CSV files with raw data are also available in `eval/results/` for further analysis.

## 6. Expected Variation

- **Absolute timing values** will differ from those reported in the paper due to hardware differences. The paper's experiments were conducted on a specific machine; your Docker container will have different CPU characteristics.
- **Relative ordering** of benchmarks by time should be approximately consistent.
- **Sat/unsat results** must match the paper exactly. Every benchmark in `positive_example/` and `nested_arrays/` should report `sat`; every benchmark in `negative_example/` should report `unsat`.
- **Z3 version comparison trends** (Table 5) should be directionally consistent: the same benchmarks that are faster on one Z3 version in the paper should generally remain faster on that version in your run.
- **Ownership term solutions** (Table 2) should match the paper exactly, as these are determined by the constraint solver and are not timing-dependent.

## 7. Directory Structure

```
nested_array_ConSORT/
  myproject/
    bin/main.ml              -- Entry point (CLI argument parsing)
    lib/                     -- Core library modules
      syntax.ml              -- AST types
      parser.mly / lexer.mll -- Parser and lexer
      simpleTyping.ml        -- Simple type inference
      generateSmtlibConstraint.ml  -- Ownership constraint generation
      CHCgenerateSmtlibConstraint.ml -- CHC constraint generation
      output.ml              -- Main workflow orchestration
    example/
      nested_arrays/         -- 19 nested array benchmarks (Table 1)
      int_arrays/            -- 8 integer array benchmarks (Table 3)
      benchmark_nested_array_in_paper/ -- Paper benchmarks (subset)
      positive_example/      -- Programs expected to pass
      negative_example/      -- Programs expected to fail
    eval/
      run_all.sh             -- Master evaluation script
      run_short.sh           -- Quick kick-the-tires evaluation
      setup.sh               -- Dependency installation and build
      table1.sh ... table5.sh -- Individual table reproduction scripts
      lib/common.sh          -- Shared helper functions
      z3-versions/           -- Z3 binaries (4.11.2 and 4.14.1)
      Extended_ConSORT/      -- Tanaka et al. tool (for Table 3 comparison)
      results/               -- Generated result files
    experiment/              -- Intermediate solver files (generated at runtime)
```

## 8. Troubleshooting

### "busy" error when running dune

If you see a "busy" error from dune, use an alternative build directory:

```sh
dune exec --build-dir="_tmp" myproject -- ./example/nested_arrays/indexed_value.imp
```

### Re-running individual benchmarks

To verify a single benchmark manually:

```sh
cd myproject
dune exec myproject -- ./example/nested_arrays/swap.imp
```

You should see output including `ownership: sat` and `refinement: sat` (or `unsat` for negative examples).

### Z3 or hoice not found

Ensure both solvers are on `$PATH`. Inside the Docker container, they should already be installed. If running outside Docker:

```sh
which z3      # Should print the path to z3
which hoice   # Should print the path to hoice
```

### Table 4 takes too long

Use fewer runs:

```sh
NUM_RUNS=2 bash eval/table4.sh
```

Or run only a subset of benchmarks by editing `eval/table4.sh` to include fewer files.

### Solver timeout or unexpected unsat

If a benchmark that should pass reports `unsat` or times out:

1. Check that Z3 version 4.14.1 is active: the tool expects this version by default.
2. Run with verbose output to see intermediate solver results in `experiment/`.
3. Ensure `experiment/own_result/` directory exists: `mkdir -p experiment/own_result`.

### Container runs out of memory

The evaluation requires at least 4 GB of RAM. If benchmarks are being killed, increase Docker's memory allocation in Docker Desktop settings.
