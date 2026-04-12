#!/usr/bin/env bash
# build-artifact.sh — Build the complete ECOOP 2026 artifact package
#
# Creates artifact-package/ containing:
#   - Docker image tar.gz (with arch suffix)
#   - Source zip (from git archive)
#   - DARTS artifact description PDF
#   - README.md, ARTIFACT.md, LICENSE
#
# Supports both x86_64 (amd64) and ARM64 (aarch64/Apple Silicon) platforms.
#
# Usage:
#   ./build-artifact.sh              # auto-detect host architecture
#   ./build-artifact.sh linux/arm64  # override platform
#   DOCKER_PLATFORM=linux/arm64 ./build-artifact.sh  # override via env
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR"

IMAGE_NAME="nested-array-consort"
IMAGE_TAG="ecoop26"
FULL_TAG="${IMAGE_NAME}:${IMAGE_TAG}"
OUTPUT_DIR="artifact-package"

# Detect host architecture
HOST_ARCH=$(uname -m)
case "$HOST_ARCH" in
    x86_64|amd64)   PLATFORM="linux/amd64"; ARCH_LABEL="x86" ;;
    aarch64|arm64)   PLATFORM="linux/arm64"; ARCH_LABEL="arm64" ;;
    *)
        echo "WARNING: Unknown architecture '$HOST_ARCH'. Defaulting to linux/amd64."
        PLATFORM="linux/amd64"; ARCH_LABEL="x86"
        ;;
esac

# Allow override via environment variable or first argument
if [ -n "${DOCKER_PLATFORM:-}" ]; then
    PLATFORM="$DOCKER_PLATFORM"
elif [ -n "${1:-}" ]; then
    PLATFORM="$1"
fi

# Derive arch label from platform
case "$PLATFORM" in
    linux/arm64) ARCH_LABEL="arm64" ;;
    *)           ARCH_LABEL="x86" ;;
esac

echo "============================================================"
echo "  Building ECOOP 2026 Artifact: ${FULL_TAG}"
echo "  Platform: ${PLATFORM} (${ARCH_LABEL})"
echo "============================================================"
echo ""

mkdir -p "$OUTPUT_DIR"

# ---- Step 1: Build Docker image ----
echo "--- Step 1/6: Building Docker image ---"
echo "  This may take 10-20 minutes (compiling hoice from source is slow)."
echo ""

docker build --platform "$PLATFORM" -t "$FULL_TAG" .

echo ""
echo "  [OK] Docker image built: $FULL_TAG"

# ---- Step 2: Smoke test ----
echo ""
echo "--- Step 2/6: Smoke test ---"

smoke_output=$(docker run --rm --platform "$PLATFORM" "$FULL_TAG" bash -c \
    'cd /home/opam/app/myproject && dune exec myproject -- ./example/nested_arrays/indexed_value.imp -unsat-core false 2>&1 | tail -5')

echo "$smoke_output"

if echo "$smoke_output" | grep -q "ownership: sat"; then
    echo "  [OK] Smoke test passed"
else
    echo "  [FAIL] Smoke test failed. Check Docker image."
    exit 1
fi

# ---- Step 3: Export Docker image ----
echo ""
echo "--- Step 3/6: Exporting Docker image ---"

TARFILE="${OUTPUT_DIR}/${IMAGE_NAME}-${IMAGE_TAG}.${ARCH_LABEL}.tar.gz"
docker save "$FULL_TAG" | gzip > "$TARFILE"

IMAGE_SIZE=$(du -h "$TARFILE" | cut -f1)
echo "  [OK] Image exported: $TARFILE ($IMAGE_SIZE)"

# ---- Step 4: Build source zip ----
echo ""
echo "--- Step 4/6: Creating source archive ---"

SOURCE_ZIP="${OUTPUT_DIR}/${IMAGE_NAME}-source.zip"
git archive --format=zip --prefix=nested_array_ConSORT/ HEAD -o "$SOURCE_ZIP"

SOURCE_SIZE=$(du -h "$SOURCE_ZIP" | cut -f1)
echo "  [OK] Source archive: $SOURCE_ZIP ($SOURCE_SIZE)"

# ---- Step 5: Build DARTS PDF ----
echo ""
echo "--- Step 5/6: Building DARTS artifact description PDF ---"

if command -v pdflatex >/dev/null 2>&1; then
    (cd darts && make) > /dev/null 2>&1
    if [ -f "${OUTPUT_DIR}/darts-artifact.pdf" ]; then
        echo "  [OK] DARTS PDF: ${OUTPUT_DIR}/darts-artifact.pdf"
    else
        echo "  [WARN] pdflatex ran but PDF not found in ${OUTPUT_DIR}/."
        echo "         Check darts/Makefile copies to ${OUTPUT_DIR}/."
    fi
else
    if [ -f "darts/darts-artifact.pdf" ]; then
        cp darts/darts-artifact.pdf "${OUTPUT_DIR}/"
        echo "  [OK] Copied existing DARTS PDF (pdflatex not available)"
    else
        echo "  [SKIP] pdflatex not available and no existing PDF found"
    fi
fi

# ---- Step 6: Copy supporting files ----
echo ""
echo "--- Step 6/6: Copying supporting files ---"

cp README.md "${OUTPUT_DIR}/"
cp ARTIFACT.md "${OUTPUT_DIR}/"
cp LICENSE "${OUTPUT_DIR}/" 2>/dev/null || echo "  WARNING: No LICENSE file found"

echo "  [OK] Supporting files copied"

# ---- Summary ----
echo ""
echo "============================================================"
echo "  Artifact package ready: ${OUTPUT_DIR}/"
echo ""
echo "  Contents:"
ls -lh "${OUTPUT_DIR}/"
echo ""
echo "  To test locally:"
echo "    docker load -i ${TARFILE}"
echo "    docker run -it --platform ${PLATFORM} ${FULL_TAG}"
echo "    # Inside container:"
echo "    bash eval/run_short.sh   # Quick test (~10 min)"
echo "    bash eval/run_all.sh     # Full evaluation (~2 hours)"
echo "============================================================"
