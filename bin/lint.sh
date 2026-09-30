#!/usr/bin/env bash
#
# Run hlint and fourmolu across all Haskell source directories.
#
# Usage:
#   bin/lint.sh          Apply hlint suggestions and reformat with fourmolu
#   bin/lint.sh --check  Check only (fails on issues, suitable for CI)
#
# Tool resolution (for hlint, fourmolu and refactor):
#   1. Use HLINT_EXE / FOURMOLU_EXE / REFACTOR_EXE environment variable if set
#   2. Otherwise use hlint / fourmolu / refactor from PATH
#   3. If not found, install via cabal
#
# refactor (from apply-refact) is only needed to apply hlint suggestions.
#
# Directories linted: src, test
#
# Examples:
#   bin/lint.sh
#   bin/lint.sh --check
#   HLINT_EXE=/opt/hlint/bin/hlint bin/lint.sh --check
#   FOURMOLU_EXE="$HOME/.local/bin/fourmolu" bin/lint.sh

set -euo pipefail

# CDPATH makes cd print the directory, which breaks $(cd ... && pwd).
unset CDPATH

script_dir=$(dirname "$0")
root_dir=$(cd "${script_dir}/.." && pwd)

check_mode=false
case "${1:-}" in
  "") ;;
  --check) check_mode=true ;;
  *)
    echo "usage: $0 [--check]" >&2
    exit 2
    ;;
esac

# resolve_tool VAR EXE PACKAGE: print the command for EXE, installing PACKAGE if needed.
resolve_tool() {
  local override="${!1:-}"
  if [ -n "${override}" ]; then
    echo "${override}"
  elif command -v "$2" >/dev/null 2>&1; then
    echo "$2"
  else
    echo "$2 not found, installing $3..." >&2
    cabal install "$3" --install-method=copy --overwrite-policy=always >&2
    echo "$2"
  fi
}

hlint_cmd=$(resolve_tool HLINT_EXE hlint hlint)
fourmolu_cmd=$(resolve_tool FOURMOLU_EXE fourmolu fourmolu)
if [ "${check_mode}" = false ]; then
  refactor_cmd=$(resolve_tool REFACTOR_EXE refactor apply-refact)
fi

cd "${root_dir}"

dirs=()
for d in src test; do
  if [ -d "${d}" ]; then
    dirs+=("${d}")
  fi
done

files=()
while IFS= read -r -d '' f; do
  files+=("${f}")
done < <(find "${dirs[@]}" -name '*.hs' -type f -print0 | sort -z)

if [ "${#files[@]}" -eq 0 ]; then
  echo "No Haskell files found." >&2
  exit 1
fi

fail=0

echo "=== hlint ==="
if [ "${check_mode}" = false ]; then
  # refactor skips overlapping hints, so repeat until the file stops changing.
  for f in "${files[@]}"; do
    for _ in 1 2 3 4 5 6 7 8 9 10; do
      before=$(cksum <"${f}")
      "${hlint_cmd}" "${f}" --refactor --with-refactor="${refactor_cmd}" --refactor-options="--inplace" || fail=1
      if [ "$(cksum <"${f}")" = "${before}" ]; then
        break
      fi
    done
  done
fi
# Report any hints that remain (in fix mode, those that could not be applied).
"${hlint_cmd}" "${dirs[@]}" || fail=1

echo "=== fourmolu ==="
if [ "${check_mode}" = true ]; then
  fourmolu_mode="check"
else
  fourmolu_mode="inplace"
fi
"${fourmolu_cmd}" --quiet --mode "${fourmolu_mode}" "${files[@]}" || fail=1

if [ "${fail}" -ne 0 ]; then
  echo "Lint checks failed." >&2
  exit 1
fi

if [ "${check_mode}" = true ]; then
  echo "All lint checks passed."
else
  echo "All fixes applied."
fi
