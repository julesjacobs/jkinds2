#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"

# Always use the build artifact via dune exec so the script reflects latest code
run_cli() {
  (cd "$ROOT_DIR" && dune exec --display=quiet -- ./bin/main.exe "$@")
}

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
  # Write file content trimmed of trailing blank lines
  sed -e :a -e '/^[[:space:]]*$/{$d;N;ba' -e '}' "$f" >>"$TMP_OUT"
  # Ensure exactly one newline after content (without creating extras)
  if [ -n "$(tail -c1 "$TMP_OUT")" ]; then echo >>"$TMP_OUT"; fi
  {
    echo '```'
    echo
    echo "Program output:"
    echo '```'
  } >>"$TMP_OUT"

  if [[ -n "$MAX_ITERS_FLAG" ]]; then
    run_cli "$f" --max-iters "$MAX_ITERS_FLAG" | sed -e :a -e '/^[[:space:]]*$/{$d;N;ba' -e '}' >>"$TMP_OUT"
  else
    run_cli "$f" | sed -e :a -e '/^[[:space:]]*$/{$d;N;ba' -e '}' >>"$TMP_OUT"
  fi
  # Ensure exactly one newline after program output
  if [ -n "$(tail -c1 "$TMP_OUT")" ]; then echo >>"$TMP_OUT"; fi
  echo '```' >>"$TMP_OUT"

  # Extras: Infer6/Infer8 leq and round_up
  {
    echo
    echo "Extras (Infer6/Infer8 leq and round_up):"
    echo '```'
  } >>"$TMP_OUT"
  (cd "$ROOT_DIR" && dune exec --display=quiet -- ./bin/report_kinds.exe "$f") \
    | sed -e :a -e '/^[[:space:]]*$/{$d;N;ba' -e '}' >>"$TMP_OUT"
  if [ -n "$(tail -c1 "$TMP_OUT")" ]; then echo >>"$TMP_OUT"; fi
  echo '```' >>"$TMP_OUT"
done

mv "$TMP_OUT" "$OUT_PATH"
echo "Wrote report to $OUT_PATH"
