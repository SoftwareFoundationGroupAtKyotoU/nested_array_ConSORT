# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

nested_array_ConSORT is a research tool for automated ownership type inference and refinement type checking for imperative programs. It extends [ConSORT](https://www.fos.kuis.kyoto-u.ac.jp/projects/consort/) with support for nested arrays. Programs are written in a custom `.imp` language.

## Build & Run

All commands run from `myproject/`:

```bash
# Build
dune build

# Run ownership + refinement inference on a .imp file
dune exec myproject -- ./example/positive_example/init_10.imp

# If "busy" error occurs
dune exec --build-dir="_tmp" myproject -- ./example/positive_example/init_10.imp

# Run tests (OUnit2)
dune test

# Run batch verification on a directory of .imp files
make run DIR=./example/positive_example
make run DIR=./example/omit_alias OPTS="-insert_alias"
make run_all   # runs positive_example, negative_example, much_time_example, omit_alias

# Format code
dune fmt
```

### CLI Flags

| Flag | Purpose |
|---|---|
| `-full_annotated` | Skip ownership inference; assume fully annotated functions |
| `-refinement` | Refinement checking only (ownership must be done already) |
| `-insert_alias` | Auto-insert alias statements (correctness not guaranteed) |
| `-random_assignment` | Disable heuristics in ownership inference |
| `-unsat-core true/false` | Enable/disable unsat core analysis (default: true) |
| `-print_program` | Pretty-print the parsed program AST |

## External Solver Dependencies

- **Z3** (SMT solver, tested with 4.14.1) - required for ownership constraint solving
- **Hoice** (CHC solver, tested with 1.10.0) - required for refinement checking
- Alternative: Eldarica can replace Hoice for refinement

Both must be on `$PATH`. The tool invokes them via `Sys.command`.

## Architecture

### Execution Pipeline

```
.imp file → Parse → Simple Type Inference → Ownership Constraint Generation
→ Z3 (iterative, adds counterexamples on "unknown") → Free Variable Extraction
→ CHC Generation → Hoice → Report sat/unsat
```

### Key Modules (in `myproject/lib/`)

- **syntax.ml** — Core AST types: `simpleTy`, `exp`, `ftype` (refinement types), `program`
- **parser.mly / lexer.mll** — Menhir parser and ocamllex lexer for `.imp` syntax
- **simpleTyping.ml** — Simple type inference pass
- **generateSmtlibConstraint.ml** — Ownership constraint generation → `experiment/out_int.smt2`
- **ownConstraintSyntax.ml** — Ownership constraint AST (`constr`, `own_represent`)
- **collectOwnConstraint.ml** — Constraint collection from AST
- **printOwnConstraint.ml** — Pretty-printing constraints
- **output.ml** — Main workflow orchestration (`generate_constrs`, `main_fv`, `main_sat_ans`, `main_chc`)
- **CHCSyntax.ml / CHCgenerateSmtlibConstraint.ml** — CHC constraint generation → `experiment/out_chc.smt2`
- **owntoCHC.ml** — Convert ownership results to CHC phase
- **cexample.ml** — Counterexample generation for iterative refinement
- **insertAlias.ml** — Automatic alias insertion
- **elaborate.ml** — Program elaboration pass
- **z3Syntax.ml, z3Parser.mly, z3Lexer.mll** — Parse Z3 output (two variants: v1 and v2)
- **smtlibSyntax.ml** — SMT-LIB data types
- **util.ml** — Shared utilities

### Entry Point

`myproject/bin/main.ml` — CLI argument parsing and mode dispatch (Normal, PrintProgram, Refinement, FullAnnotated).

### Generated Files (in `experiment/`, git-ignored)

- `out_int.smt2` — Ownership constraints (Z3 input)
- `result_int` — Z3 output
- `out_fv.smt2` — Free variable phase (Z3 input)
- `out_chc.smt2` — CHC constraints (Hoice input)
- `chc_result` — Hoice output
- `own_result/result_*` — Per-file ownership solutions
- `out_sat_ans.amt2` — Final ownership answer

## Language Notes

- Comments and README are primarily in Japanese
- OCaml 4.14.1, Dune 2.7+, Menhir for parser generation
- `ppx_deriving.std` for `[@@deriving show]` on types
- Warnings `-warn-error -A -w -39` (all warnings non-fatal, unused rec flag suppressed)
- `.ocamlformat` with `comment-check=false`

## Example Programs

Test `.imp` files are in `myproject/example/`:
- `positive_example/` — Programs that should pass verification
- `negative_example/` — Programs expected to fail
- `int_arrays/` — Integer array benchmarks
- `nested_arrays/` — Nested array examples
- `benchmark_nested_array_in_paper/` — Benchmarks from the paper
- `much_time_example/` — Long-running verification cases
- `omit_alias/` — Examples without alias annotations (use with `-insert_alias`)
