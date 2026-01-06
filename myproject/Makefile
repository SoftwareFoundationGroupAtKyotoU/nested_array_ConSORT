# -----------------------------------------------------------------------------
# 汎用テスト実行Makefile
#
# 使い方:
#   - 単一ディレクトリ実行: make run DIR=<テストケースのディレクトリパス>
#   - 全ディレクトリ実行:   make run_all
#   - ログの削除:         make clean
#
# 例:
#   make run DIR=./example/positive_example
#   make run_all
# -----------------------------------------------------------------------------

# 'make run_all' で実行するディレクトリのリスト
ALL_DIRS = \
	./example/positive_example \
	./example/negative_example \
	./example/much_time_example \
	./example/omit_alias

# デフォルトで実行するディレクトリ (make run のみの場合)
DEFAULT_DIR = ./example/positive_example
DIR ?= $(DEFAULT_DIR)

OPTS ?=

# DIRからディレクトリ名を取得し、ログファイル名を生成 (例: ./a/b -> logs/b.log)
DIR_BASENAME := $(notdir $(patsubst %/,%,$(DIR)))
LOG_FILE := logs/$(DIR_BASENAME).log

# 'run_all' 用のログファイルリストを生成
ALL_LOG_FILES := $(patsubst %, logs/%.log, $(notdir $(patsubst %/,%,$(ALL_DIRS))))

# 検証の上限時間
TIMEOUT_SEC = 1000

# .PHONY は、ファイル名とターゲット名が衝突しても正しく動作させるための宣言
.PHONY: run run_all clean help default

.PRECIOUS: $(ALL_LOG_FILES)

# デフォルトターゲット (make とだけ打った時に実行される)
default: help

# ---- メインのターゲット ----

# 全ての指定ディレクトリを実行するターゲット
run_all: $(ALL_LOG_FILES)
	@echo "============================================================"
	@echo "All specified directories have been processed."
	@echo "Generated logs: $(ALL_LOG_FILES)"
	@echo "============================================================"

# 'run_all' から呼び出される、ログファイル名をターゲットとしたルール
# 例: 'make logs/positive_example.log' が実行される
$(ALL_LOG_FILES):
	@# このルールは 'run_all' ターゲットから呼び出される
	@# 'make' コマンドを再帰的に呼び出すことで、単一実行のロジックを再利用する
	$(MAKE) run DIR=./example/$(patsubst %.log,%,$(notdir $@)) OPTS="$(OPTS)"

# 単一のディレクトリを実行するターゲット
run:
	@# ログを保存するディレクトリがなければ作成
	@mkdir -p $(dir $(LOG_FILE))
	@echo "============================================================"
	@echo "Target Directory : $(DIR)"
	@echo "Log File         : $(LOG_FILE)"
	@echo "============================================================"
	@( \
	    echo "==> Changing directory to ~/nested_array_ConSORT/myproject..." && \
	    cd ~/nested_array_ConSORT/myproject && \
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


# 生成されたログファイルを削除するターゲット
clean:
	@echo "Cleaning up log files in logs/ directory..."
	@rm -rf logs
	@echo "Done."

# ヘルプメッセージを表示するターゲット
help:
	@echo "Usage:"
	@echo "  make run DIR=<path>  - Run tests in a single directory <path>."
	@echo "  make run_all         - Run tests for all predefined directories."
	@echo "                         ($(ALL_DIRS))"
	@echo "  make clean           - Remove all generated log files."
	@echo ""
	@echo "Examples:"
	@echo "  make run DIR=./example/positive_example"
	@echo "  make run_all"
