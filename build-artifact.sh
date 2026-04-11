#!/usr/bin/env bash
# build-artifact.sh — Build and export the Docker image for ECOOP 2026 artifact evaluation
set -euo pipefail

IMAGE_NAME="nested-array-consort"
IMAGE_TAG="ecoop26"
FULL_TAG="${IMAGE_NAME}:${IMAGE_TAG}"
OUTPUT_DIR="artifact-package"

echo "============================================================"
echo "  Building ECOOP 2026 Artifact: ${FULL_TAG}"
echo "============================================================"
echo ""

# ---- Step 1: Build Docker image ----
echo "--- Step 1: Building Docker image ---"
echo "  This may take 10-20 minutes (compiling hoice from source is slow)."
echo ""

docker build --platform linux/amd64 -t "$FULL_TAG" .

echo ""
echo "  [OK] Docker image built: $FULL_TAG"

# ---- Step 2: Quick smoke test inside the container ----
echo ""
echo "--- Step 2: Smoke test ---"

smoke_output=$(docker run --rm --platform linux/amd64 "$FULL_TAG" bash -c \
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
echo "--- Step 3: Exporting Docker image ---"

mkdir -p "$OUTPUT_DIR"

TARFILE="${OUTPUT_DIR}/${IMAGE_NAME}-${IMAGE_TAG}.tar.gz"
docker save "$FULL_TAG" | gzip > "$TARFILE"

IMAGE_SIZE=$(du -h "$TARFILE" | cut -f1)
echo "  [OK] Image exported: $TARFILE ($IMAGE_SIZE)"

# ---- Step 4: Copy supporting files ----
echo ""
echo "--- Step 4: Copying supporting files ---"

cp ARTIFACT.md "${OUTPUT_DIR}/"
cp LICENSE "${OUTPUT_DIR}/" 2>/dev/null || echo "  WARNING: No LICENSE file found"
cp README.md "${OUTPUT_DIR}/"

echo "  [OK] Supporting files copied"

# ---- Summary ----
echo ""
echo "============================================================"
echo "  Artifact package ready in: ${OUTPUT_DIR}/"
echo ""
echo "  Contents:"
ls -lh "${OUTPUT_DIR}/"
echo ""
echo "  To test locally:"
echo "    docker load -i ${TARFILE}"
echo "    docker run -it --platform linux/amd64 ${FULL_TAG}"
echo "    # Inside container:"
echo "    bash eval/run_short.sh   # Quick test (~10 min)"
echo "    bash eval/run_all.sh     # Full evaluation (~2-4 hours)"
echo ""
echo "  Next steps:"
echo "    1. Upload ${OUTPUT_DIR}/ contents to Zenodo"
echo "    2. Add DARTS artifact description PDF"
echo "    3. Submit DOI to ECOOP"
echo "============================================================"
