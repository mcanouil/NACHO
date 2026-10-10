#!/usr/bin/env bash
set -euo pipefail

# Compare the Status line of an R CMD check log with the results line of
# cran-comments.md, so the file sent to CRAN never reports fewer problems
# than the check found.
# Usage: check-results.sh <00check.log> [cran-comments.md]

log="${1:?Usage: check-results.sh <00check.log> [cran-comments.md]}"
comments="${2:-cran-comments.md}"

for file in "${log}" "${comments}"; do
  if [ ! -f "${file}" ]; then
    echo "::error::${file} does not exist." >&2
    exit 1
  fi
done

status="$(tr -d '\r' <"${log}" | sed -n 's/^Status:[[:space:]]*//p' | tail -n 1)"
if [ -z "${status}" ]; then
  echo "::error::${log} has no Status line, so the check did not finish." >&2
  exit 1
fi

count() {
  local n
  n="$(grep -oE "[0-9]+ $1" <<<"${status}" | grep -oE '^[0-9]+' || true)"
  echo "${n:-0}"
}

found="$(count ERROR) $(count WARNING) $(count NOTE)"
claimed="$(tr -d '\r' <"${comments}" |
  sed -nE 's/^([0-9]+) errors? \| ([0-9]+) warnings? \| ([0-9]+) notes?[[:space:]]*$/\1 \2 \3/p' |
  head -n 1)"

if [ -z "${claimed}" ]; then
  echo "::error::${comments} has no line like '0 errors | 0 warnings | 0 notes'." >&2
  exit 1
fi

read -r errors warnings notes <<<"${found}"
read -r claimed_errors claimed_warnings claimed_notes <<<"${claimed}"
summary="${errors} errors | ${warnings} warnings | ${notes} notes"
claimed_summary="${claimed_errors} errors | ${claimed_warnings} warnings | ${claimed_notes} notes"
if [ "${claimed_errors}" -lt "${errors}" ] ||
  [ "${claimed_warnings}" -lt "${warnings}" ] ||
  [ "${claimed_notes}" -lt "${notes}" ]; then
  {
    echo "::error::${comments} reports fewer problems than the check found. The check found ${summary}, but ${comments} says ${claimed_summary}. Explain each problem in ${comments}, correct its results line, and run the CRAN submission again."
    tr -d '\r' <"${log}" | grep -E '\.\.\. .*(NOTE|WARNING|ERROR)$' || true
  } >&2
  exit 1
fi

echo "${comments} reports at least what the check found: check ${summary}, ${comments} ${claimed_summary}"
