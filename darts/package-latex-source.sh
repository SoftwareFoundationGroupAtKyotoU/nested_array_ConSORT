#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$script_dir"

archive_name="${1:-darts-latex-source.zip}"
case "$archive_name" in
  /*) archive_path="$archive_name" ;;
  *) archive_path="$script_dir/$archive_name" ;;
esac

package_dir="darts-latex-source"
tmp_dir="$(mktemp -d "${TMPDIR:-/tmp}/darts-latex-source.XXXXXX")"
trap 'rm -rf "$tmp_dir"' EXIT

files=(
  "darts-artifact.tex"
  "references.bib"
  "darts-v2021.cls"
  "darts-logo-bw.pdf"
  "cc-by.pdf"
  "orcid.pdf"
)

missing=0
for file in "${files[@]}"; do
  if [[ ! -f "$file" ]]; then
    printf 'missing required file: %s\n' "$file" >&2
    missing=1
  fi
done

if [[ "$missing" -ne 0 ]]; then
  exit 1
fi

mkdir -p "$tmp_dir/$package_dir"
for file in "${files[@]}"; do
  cp "$file" "$tmp_dir/$package_dir/"
done

rm -f "$archive_path"
(
  cd "$tmp_dir"
  zip -qr "$archive_path" "$package_dir"
)

printf 'created %s\n' "$archive_path"
