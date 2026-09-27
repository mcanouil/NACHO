#!/usr/bin/env bash
set -euo pipefail

version="${1:?Usage: news-section.sh <version> [NEWS.md|-] [package]}"
news="${2:-NEWS.md}"
package="${3:-NACHO}"
heading="# ${package} ${version}"

section="$(
  awk -v heading="${heading}" '
    flag && /^# / { exit }
    $0 == heading { flag = 1; next }
    flag
  ' "${news}" | sed '/./,$!d'
)"

if [ -z "${section//[[:space:]]/}" ]; then
  echo "::error::No '${heading}' section with content in ${news}." >&2
  exit 1
fi

printf '%s\n' "${section}"
