#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
CLI="$ROOT_DIR/_build/install/default/bin/jkinds"

if [[ ! -x "$CLI" ]]; then
  echo "Building jkinds CLI..." >&2
  (cd "$ROOT_DIR" && dune build @install >/dev/null)
fi

TYPES_DIR="$ROOT_DIR/test/types"
MAX_ITERS_FLAG=${MAX_ITERS_FLAG:-}

OUT_PATH="${1:-$ROOT_DIR/types_report.md}"
TMP_OUT="$(mktemp)"

{
  echo "# jkinds Types Report"
  echo
  echo "Generated: $(date -u '+%Y-%m-%d %H:%M:%S UTC')"
} >"$TMP_OUT"

shopt -s nullglob
FILES=("$TYPES_DIR"/*.types)

for f in "${FILES[@]}"; do
  base="$(basename "$f")"
  {
    echo
    echo "## $base"
    echo
    echo '```'
  } >>"$TMP_OUT"
  cat "$f" >>"$TMP_OUT"
  {
    echo '```'
    echo
    echo "Program output:"
    echo '```'
  } >>"$TMP_OUT"

  if [[ -n "$MAX_ITERS_FLAG" ]]; then
    "$CLI" "$f" --max-iters "$MAX_ITERS_FLAG" >>"$TMP_OUT"
  else
    "$CLI" "$f" >>"$TMP_OUT"
  fi

  echo '```' >>"$TMP_OUT"
done

mv "$TMP_OUT" "$OUT_PATH"
echo "Wrote report to $OUT_PATH"

