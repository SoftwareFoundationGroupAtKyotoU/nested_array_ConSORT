# Artifact Documentation: Ownership Refinement Types for Pointer Arithmetic and Nested Arrays

ECOOP 2026 Artifact Evaluation

## 1. Overview

This artifact accompanies the paper "Ownership Refinement Types for Pointer Arithmetic and Nested Arrays". It provides `nested_array_ConSORT`, a tool for automated ownership type inference and refinement type checking for imperative programs with pointer arithmetic and nested arrays, extending [ConSORT](https://www.fos.kuis.kyoto-u.ac.jp/projects/consort/).

The artifact supports the following claims from the paper:

- **Table 1**: Verification timing and alias statement counts for 19 nested array benchmarks.
- **Table 3**: Performance comparison with Tanaka et al. on 8 integer array benchmarks.

## 2. Requirements

- **Docker** (tested with Docker 20.x and later)
- Approximately **4 GB** of disk space for the Docker image
- **x86_64** Linux or macOS with Docker support
- No internet connection required after loading the image

## 3. Getting Started (Kick-the-Tires, 30 minutes or less)

### Step 1: Load the Docker image

If you have the pre-built image:

```sh
docker load -i nested-array-consort-ecoop26.tar.gz
```

Alternatively, you can build the image from source:

```sh
git clone https://github.com/SoftwareFoundationGroupAtKyotoU/nested_array_ConSORT.git
cd nested_array_ConSORT
docker build --platform linux/amd64 -t nested-array-consort:ecoop26 .
```

Note: Building from source requires an internet connection (to download Z3, hoice, and Extended_ConSORT) and may take 10-15 minutes.

### Step 2: Start the container

```sh
docker run -it --platform linux/amd64 nested-array-consort:ecoop26
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
- Final summary indicating which paper tables correspond to each output.

If all benchmarks report `ownership: sat` and `refinement: sat`, the tool is working correctly.

## 4. Full Evaluation (~30 minutes)

### Run all tables at once

```sh
bash eval/run_all.sh
```

This script runs the table reproduction scripts (Tables 1 and 3) sequentially and stores results in `eval/results/`.

### Run individual tables

If you prefer to run tables independently:

```sh
bash eval/table1.sh    # Table 1: Nested array benchmarks (~10 min)
bash eval/table3.sh    # Table 3: Comparison with Tanaka et al. (~10 min)
```

## 5. Claims and Evidence Mapping

| Paper Section | Script | Output File | What to Check |
|---|---|---|---|
| Table 1 | `eval/table1.sh` | `eval/results/table1.md` | Timing and alias counts for 19 nested array benchmarks |
| Table 3 | `eval/table3.sh` | `eval/results/table3.md` | Timing comparison with Tanaka et al. on 8 integer array benchmarks |

CSV files with raw data are also available in `eval/results/` for further analysis.

## 6. Expected Variation

- **Absolute timing values** will differ from those reported in the paper due to hardware differences. The paper's experiments were conducted on a specific machine; your Docker container will have different CPU characteristics.
- **Relative ordering** of benchmarks by time should be approximately consistent.
- **Sat/unsat results** must match the paper exactly. Every benchmark in `positive_example/` and `nested_arrays/` should report `sat`; every benchmark in `negative_example/` should report `unsat`.


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
      table1.sh, table3.sh -- Individual table reproduction scripts
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

### Trace-Matrix refinement timeout

The Trace-Matrix benchmark's refinement phase (hoice) may take longer than the default timeout (900s). This is a known solver performance issue, not a bug in the tool. The ownership phase completes successfully.

### Solver timeout or unexpected unsat

If a benchmark that should pass reports `unsat` or times out:

1. Check that Z3 version 4.14.1 is active: the tool expects this version by default.
2. Run with verbose output to see intermediate solver results in `experiment/`.
3. Ensure `experiment/own_result/` directory exists: `mkdir -p experiment/own_result`.

### Container runs out of memory

The evaluation requires at least 4 GB of RAM. If benchmarks are being killed, increase Docker's memory allocation in Docker Desktop settings.
