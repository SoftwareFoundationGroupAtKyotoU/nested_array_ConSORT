# ECOOP 2026 Artifact Evaluation Package — Plan

## Overview

Create a self-contained artifact evaluation package for ECOOP 2026 paper
"Ownership Refinement Types for Pointer Arithmetic and Nested Arrays".

## 1. Fix Dockerfile

The current Dockerfile has several issues that need fixing:

| Issue | Fix |
|---|---|
| Z3 URL hardcoded to `arm64` | Use `x64-glibc-2.35` for Docker (x86_64 Linux) |
| Only Z3 4.14.1, missing 4.11.2 | Install both to `/usr/local/z3-{version}/bin/z3` |
| hoice uses default Rust (too new) | Pin `rustup default 1.78.0` before `cargo install` |
| Missing Extended_ConSORT | Clone + build inside image |
| Missing eval scripts | Already in repo, just ensure they're copied |
| Missing `experiment/own_result` dir | Add `mkdir -p` before build |

**Target**: Single `docker run` reproduces everything with no internet required.

## 2. Create `run_short.sh` (Kick-the-Tires, ≤10 minutes)

For the initial "does it work?" evaluation phase:
- Run 3-4 fast benchmarks from Table 1 (Init-Matrix(2D), Indexed-Value, Swap)
- Run 1 benchmark from Table 3 (Init-10, both our tool and Tanaka+)
- Print a summary showing "these outputs correspond to Tables 1 and 3 in the paper"

## 3. Documentation for Evaluators — `ARTIFACT.md`

Create in repo root:
- **Getting Started** (≤30 min): Docker pull/load, run `run_short.sh`
- **Full Evaluation** (2-4 hours): run `run_all.sh`
- **Claims Mapping**: Which script → which table → which paper claim
- **Expected Variation**: Timing will differ from paper; structure/sat-unsat results should match

## 4. DARTS Artifact Description (LaTeX PDF)

Create LaTeX document using the DARTS template:
- Abstract describing the tool
- Scope: Tables 1-3 reproducibility
- Content: Docker image + source
- Getting started instructions
- Detailed evaluation instructions
- Link claims ↔ scripts

## 5. Zenodo Upload

- Export Docker image: `docker save -o artifact.tar.gz nested-array-consort:ecoop26`
- Upload to Zenodo with:
  - `artifact.tar.gz` (Docker image)
  - Source tarball
  - DARTS PDF
  - `LICENSE`
- Get DOI for submission

## 6. Implementation Order

| Step | Task | Dependency |
|---|---|---|
| A | Update Dockerfile | Wait for `run_all.sh` results to confirm scripts work |
| B | Build + test Docker image locally | A |
| C | Create `run_short.sh` | Scripts finalized |
| D | Create `ARTIFACT.md` | B, C |
| E | Write DARTS LaTeX | D |
| F | Test full pipeline in Docker | B, C |
| G | Upload to Zenodo | F, E |

## 7. Key Decisions

1. **Docker base image**: Keep `ocaml/opam:ubuntu-22.04-ocaml-4.14` (recommended)
2. **Multi-arch**: x86_64 only (evaluators typically use x86_64 Linux)
3. **Delivery**: Pre-built Docker image via `docker save` (ECOOP prefers pre-built)

## 8. Known Issues to Address

- OCaml >= 4.13 required (SIGSTKSZ issue on glibc >= 2.34)
- hoice v1.10.0 requires Rust <= 1.78.0
- Z3 version switching via PATH (tool uses bare `z3` in Sys.command)
- Sequential execution mandatory (tool writes to hardcoded `experiment/` paths)
- Extended_ConSORT test.sh has no timing → our `run_tanaka()` wrapper provides timing
