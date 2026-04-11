# nested_array_ConSORT

An extension of [ConSORT](https://www.fos.kuis.kyoto-u.ac.jp/projects/consort/) with support for pointer arithmetic and nested arrays.
This tool performs automated ownership type inference and refinement type checking for imperative programs written in a custom `.imp` language.

[日本語版 README はこちら](README.ja.md)

**Related repositories:**
- [Extended_ConSORT (Tanaka et al.)](https://github.com/mamizu-git/Extended_ConSORT) — the predecessor tool

## Quick Start with Docker

A pre-built Docker image is provided for artifact evaluation. No manual dependency installation is required.

```sh
# Load the image (if distributed as a tar.gz)
docker load -i nested-array-consort-ecoop26.tar.gz

# Or build it from source
docker build --platform linux/amd64 -t nested-array-consort:ecoop26 .

# Run the container
docker run -it --platform linux/amd64 nested-array-consort:ecoop26

# Inside the container: quick smoke test
bash eval/run_short.sh

# Inside the container: full evaluation (reproduces Tables 1-3 from the paper)
bash eval/run_all.sh
```

See [ARTIFACT.md](ARTIFACT.md) for detailed evaluation instructions.

## Building from Source

### Dependencies

The tool is written in OCaml and is known to work with OCaml 4.14.1 (requires >= 4.13).
It also depends on the following OCaml libraries:

* zarith
* ppx_deriving
* ounit2
* menhir

The following external solvers are required and must be on `$PATH`:

* [Z3](https://github.com/Z3Prover/z3) (tested with 4.14.1)
* [hoice](https://github.com/hopv/hoice) (tested with 1.10.0)

#### Installing hoice

hoice requires Rust. Install Rust via [rustup](https://rustup.rs/) if you haven't already.

Note: hoice v1.10.0 does not compile with Rust 1.81+. Use Rust 1.78.0:

```sh
rustup install 1.78.0
rustup run 1.78.0 cargo install --git https://github.com/hopv/hoice --tag v1.10.0 --locked
```

This places the `hoice` binary in `~/.cargo/bin/`, which should be on your `$PATH` if Rust was installed via rustup.

### Compilation

All commands should be run from the `myproject/` directory.

```sh
cd myproject
mkdir -p experiment/own_result
dune build
```

Note: The `experiment/` directory is used to store intermediate files generated during verification. It must be created before running the tool.

To clean the build artifacts:

```sh
dune clean
```

## Usage

All commands below should be run from the `myproject/` directory.

### Basic verification

Run ownership inference and refinement type checking on a `.imp` file:

```sh
dune exec myproject -- ./example/positive_example/init_10.imp
```

If you see `ownership: sat` and `refinement: sat`, it means that the ownership and refinement inference have succeeded, respectively. Results are output to `experiment/out_sat_ans.smt2`.

If you get a "busy" error:
```sh
dune exec --build-dir="_tmp" myproject -- ./example/positive_example/init_10.imp
```

### CLI options

| Flag | Purpose |
|---|---|
| `-refinement` | Refinement (assertion) checking only. Use if ownership checking has already been completed. |
| `-full_annotated` | Fully annotated mode. Use when all matrix type annotations are provided in function definitions. Expected to be faster. |
| `-random_assignment` | Disable heuristics in ownership type inference. |
| `-insert_alias` | Automatic alias insertion. Correctness of inserted aliases is not guaranteed. |
| `-print_program` | Pretty-print the parsed program AST. |
| `-unsat-core true/false` | Enable/disable unsat core analysis (default: true). |

### Batch verification

```sh
# Verify all .imp files in a specific directory
make run DIR=./example/positive_example

# Pass additional options
make run DIR=./example/omit_alias OPTS="-insert_alias"

# Verify all .imp files under positive_example/, negative_example/,
# much_time_example/, and omit_alias/
make run_all
```

### Artifact evaluation

```sh
make eval-short     # Quick smoke-test (subset of benchmarks)
make eval-all       # Full evaluation: reproduce Tables 1-3 from the paper
make eval-table1    # Reproduce Table 1 (Nested Array Benchmarks)
make eval-table2    # Reproduce Table 2 (Ownership Terms)
make eval-table3    # Reproduce Table 3 (Comparison with Tanaka et al.)
```

## Example Programs

Test `.imp` files are provided in `myproject/example/`:

| Directory | Description |
|---|---|
| `nested_arrays/` | Nested array benchmarks (Table 1 in the paper) |
| `int_arrays/` | Integer array benchmarks (Table 3 in the paper) |
| `benchmark_nested_array_in_paper/` | Subset of benchmarks featured in the paper |
| `positive_example/` | Programs expected to pass verification |
| `negative_example/` | Programs expected to fail verification |
| `much_time_example/` | Long-running verification cases |
| `omit_alias/` | Examples without alias annotations (use with `-insert_alias`) |

## Notes

- To use [Eldarica](https://github.com/uuverifiers/eldarica) instead of hoice for refinement checking, replace the hoice invocation in `main` with:
  ```
  <path-to-eldarica> -hsmt ./experiment/out_chc.smt2 > experiment/chc_result
  ```

## Repository Structure

```
nested_array_ConSORT/
  ARTIFACT.md              -- Artifact evaluation documentation
  Dockerfile               -- Docker image for artifact evaluation
  build-artifact.sh        -- Script to build and package the Docker image
  LICENSE                  -- Apache License 2.0
  darts/                   -- DARTS artifact description (LaTeX)
  verus/                   -- Verus verification code
  myproject/               -- Main tool source code
    bin/main.ml            -- Entry point (CLI argument parsing)
    lib/                   -- Core library modules
    example/               -- Benchmark programs (.imp files)
    eval/                  -- Evaluation scripts for artifact reproduction
    experiment/            -- Intermediate solver files (generated at runtime)
```

## License

This project is licensed under the Apache License 2.0. See [LICENSE](LICENSE) for details.
