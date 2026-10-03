# =============================================================================
# Makefile for nested_array_ConSORT
#
# Usage:
#   make build              - Build the project with dune
#   make test               - Run unit tests
#   make run DIR=<path>     - Run verification on all .imp files in <path>
#   make run_all            - Run verification on all predefined directories
#   make clean              - Remove generated logs and experiment artifacts
#
# Examples:
#   make run DIR=./example/positive_example
#   make run DIR=./example/omit_alias OPTS="-insert_alias"
#   make run_all
# =============================================================================

# --- Configuration ---

# Directories to process with 'make run_all'
ALL_DIRS = \
	./example/positive_example \
	./example/negative_example \
	./example/much_time_example \
	./example/omit_alias

# Default directory for 'make run'
DEFAULT_DIR = ./example/positive_example
DIR ?= $(DEFAULT_DIR)

# Extra CLI options (e.g., -insert_alias)
OPTS ?=

# Timeout for each verification run (seconds)
TIMEOUT_SEC = 1500

# Derived names for log files
DIR_BASENAME := $(notdir $(patsubst %/,%,$(DIR)))
LOG_FILE := logs/$(DIR_BASENAME).log
ALL_LOG_FILES := $(patsubst %, logs/%.log, $(notdir $(patsubst %/,%,$(ALL_DIRS))))

# --- Phony Targets ---

.PHONY: build test run run_all clean help default

.PRECIOUS: $(ALL_LOG_FILES)

default: help

# =============================================================================
# Build & Test
# =============================================================================

build:
	@mkdir -p experiment/own_result
	dune build

test:
	dune test

# =============================================================================
# Benchmark Runs (legacy batch runner)
# =============================================================================

# Run verification on all predefined directories
run_all: $(ALL_LOG_FILES)
	@echo "============================================================"
	@echo "All specified directories have been processed."
	@echo "Generated logs: $(ALL_LOG_FILES)"
	@echo "============================================================"

# Pattern rule: each log file triggers a recursive make for the matching dir
$(ALL_LOG_FILES):
	$(MAKE) run DIR=./example/$(patsubst %.log,%,$(notdir $@)) OPTS="$(OPTS)"

# Run verification on a single directory
run:
	@mkdir -p $(dir $(LOG_FILE))
	@mkdir -p experiment/own_result
	@echo "============================================================"
	@echo "Target Directory : $(DIR)"
	@echo "Log File         : $(LOG_FILE)"
	@echo "============================================================"
	@( \
	    for file in $(DIR)/*.imp; do \
	        if [ -f "$$file" ]; then \
	            echo "--- Executing for: $$file ---"; \
	            gtimeout $(TIMEOUT_SEC)s dune exec myproject -- "$$file" $(OPTS) -unsat-core false; \
	            EXIT_CODE=$$?; \
	            if [ $$EXIT_CODE -eq 124 ]; then \
	                echo "!!! TIME LIMIT EXCEEDED ($(TIMEOUT_SEC) s) - Skipped !!!"; \
	            elif [ $$EXIT_CODE -ne 0 ]; then \
	                echo "!!! Process failed with error code $$EXIT_CODE !!!"; \
	            fi; \
	            echo ""; \
	        fi; \
	    done \
	) 2>&1 | tee "$(LOG_FILE)"
	@echo "==> Processing complete. Log saved to $(LOG_FILE)."
	@echo "============================================================"

# =============================================================================
# Cleanup
# =============================================================================

clean:
	@echo "Cleaning up generated artifacts..."
	@rm -rf logs
	@rm -f experiment/out_int.smt2 experiment/result_int
	@rm -f experiment/out_fv.smt2 experiment/out_chc.smt2
	@rm -f experiment/chc_result experiment/out_sat_ans.smt2
	@rm -rf experiment/own_result
	@echo "Done."

# =============================================================================
# Help
# =============================================================================

help:
	@echo "Usage:"
	@echo "  make build              Build the project with dune"
	@echo "  make test               Run unit tests (OUnit2)"
	@echo "  make run DIR=<path>     Run verification on .imp files in <path>"
	@echo "  make run_all            Run on all predefined directories"
	@echo ""
	@echo "  make clean              Remove logs and experiment artifacts"
	@echo ""
	@echo "Options:"
	@echo "  DIR=<path>              Directory of .imp files (default: $(DEFAULT_DIR))"
	@echo "  OPTS=<flags>            Extra flags passed to the tool (e.g., -insert_alias)"
	@echo "  TIMEOUT_SEC=<n>         Per-file timeout in seconds (default: $(TIMEOUT_SEC))"
	@echo ""
	@echo "Examples:"
	@echo "  make run DIR=./example/positive_example"
	@echo "  make run DIR=./example/omit_alias OPTS=\"-insert_alias\""
