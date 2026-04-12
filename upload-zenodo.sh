#!/usr/bin/env bash
# upload-zenodo.sh — Upload artifact-package files to Zenodo
#
# Compares local files against what is already on Zenodo and uploads
# only files that are newer or missing. Skips files whose size matches.
#
# Usage:
#   ./upload-zenodo.sh                    # uses .zenodo-token file
#   ZENODO_TOKEN=xxx ./upload-zenodo.sh   # or via environment variable
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR"

RECORD_ID="19521221"
ZENODO_API="https://zenodo.org/api"
OUTPUT_DIR="artifact-package"

# --- Load token ---
if [ -n "${ZENODO_TOKEN:-}" ]; then
    TOKEN="$ZENODO_TOKEN"
elif [ -f ".zenodo-token" ]; then
    TOKEN="$(cat .zenodo-token | tr -d '[:space:]')"
else
    echo "ERROR: No Zenodo token found."
    echo "  Set ZENODO_TOKEN env var or create .zenodo-token file."
    exit 1
fi

if [ ! -d "$OUTPUT_DIR" ]; then
    echo "ERROR: $OUTPUT_DIR directory not found."
    exit 1
fi

echo "============================================================"
echo "  Zenodo Upload — Record $RECORD_ID"
echo "============================================================"
echo ""

# --- Get the latest draft deposit ---
echo "--- Fetching deposit info ---"
DEPOSIT_JSON=$(curl -s -H "Authorization: Bearer $TOKEN" \
    "$ZENODO_API/deposit/depositions/$RECORD_ID")

# Check for errors
if echo "$DEPOSIT_JSON" | python3 -c "import sys,json; d=json.load(sys.stdin); sys.exit(0 if 'id' in d else 1)" 2>/dev/null; then
    echo "  [OK] Connected to deposit $RECORD_ID"
else
    echo "  [ERROR] Failed to fetch deposit. Response:"
    echo "$DEPOSIT_JSON" | python3 -m json.tool 2>/dev/null || echo "$DEPOSIT_JSON"
    exit 1
fi

# Check if deposit is in 'draft' state (editable)
STATE=$(echo "$DEPOSIT_JSON" | python3 -c "import sys,json; print(json.load(sys.stdin).get('state','unknown'))")
if [ "$STATE" != "unsubmitted" ] && [ "$STATE" != "inprogress" ]; then
    echo ""
    echo "  Deposit state is '$STATE'. Creating new version draft..."
    NEW_VERSION_JSON=$(curl -s -X POST \
        -H "Authorization: Bearer $TOKEN" \
        "$ZENODO_API/deposit/depositions/$RECORD_ID/actions/newversion")
    DRAFT_URL=$(echo "$NEW_VERSION_JSON" | python3 -c "import sys,json; print(json.load(sys.stdin)['links']['latest_draft'])")
    DEPOSIT_JSON=$(curl -s -H "Authorization: Bearer $TOKEN" "$DRAFT_URL")
    RECORD_ID=$(echo "$DEPOSIT_JSON" | python3 -c "import sys,json; print(json.load(sys.stdin)['id'])")
    echo "  [OK] New draft created: deposit $RECORD_ID"
fi

# Get bucket URL for file uploads
BUCKET_URL=$(echo "$DEPOSIT_JSON" | python3 -c "import sys,json; print(json.load(sys.stdin)['links']['bucket'])")
echo "  Bucket: $BUCKET_URL"

# --- Get existing files on Zenodo ---
echo ""
echo "--- Checking existing files on Zenodo ---"

# Save remote file list to temp file: name|size per line
REMOTE_LIST=$(mktemp)
trap "rm -f $REMOTE_LIST" EXIT

echo "$DEPOSIT_JSON" | python3 -c "
import sys, json
d = json.load(sys.stdin)
for f in d.get('files', []):
    print(f'{f[\"filename\"]}|{f[\"filesize\"]}')
" > "$REMOTE_LIST"

if [ -s "$REMOTE_LIST" ]; then
    while IFS='|' read -r fname fsize; do
        echo "  Remote: $fname ($fsize bytes)"
    done < "$REMOTE_LIST"
else
    echo "  (no existing files)"
fi

# --- Compare and upload ---
echo ""
echo "--- Uploading files ---"

UPLOADED=0
SKIPPED=0

for filepath in "$OUTPUT_DIR"/*; do
    [ -f "$filepath" ] || continue
    filename=$(basename "$filepath")
    local_size=$(stat -f%z "$filepath" 2>/dev/null || stat -c%s "$filepath" 2>/dev/null)

    remote_size=$(grep "^${filename}|" "$REMOTE_LIST" | cut -d'|' -f2)

    if [ -n "$remote_size" ] && [ "$local_size" = "$remote_size" ]; then
        echo "  SKIP: $filename (same size: $local_size bytes)"
        SKIPPED=$((SKIPPED + 1))
        continue
    fi

    if [ -n "$remote_size" ]; then
        echo "  UPDATE: $filename (local: $local_size, remote: $remote_size)"
    else
        echo "  NEW: $filename ($local_size bytes)"
    fi

    # Upload via bucket API (PUT overwrites existing file with same name)
    HTTP_CODE=$(curl -s -o /dev/null -w "%{http_code}" \
        -X PUT \
        -H "Authorization: Bearer $TOKEN" \
        -H "Content-Type: application/octet-stream" \
        --data-binary @"$filepath" \
        "$BUCKET_URL/$filename")

    if [ "$HTTP_CODE" -ge 200 ] && [ "$HTTP_CODE" -lt 300 ]; then
        echo "    [OK] Uploaded ($HTTP_CODE)"
        UPLOADED=$((UPLOADED + 1))
    else
        echo "    [FAIL] HTTP $HTTP_CODE"
        echo "    Retrying with verbose output..."
        curl -s -X PUT \
            -H "Authorization: Bearer $TOKEN" \
            -H "Content-Type: application/octet-stream" \
            --data-binary @"$filepath" \
            "$BUCKET_URL/$filename" | python3 -m json.tool 2>/dev/null || true
    fi
done

# --- Summary ---
echo ""
echo "============================================================"
echo "  Done: $UPLOADED uploaded, $SKIPPED skipped"
echo ""
echo "  Review at: https://zenodo.org/deposit/$RECORD_ID"
echo "============================================================"
