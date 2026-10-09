#!/usr/bin/env bash
set -euo pipefail

dir="${1:-.}"

field_value() {
  sed -n "s/^$1:[[:space:]]*//p" "$2" 2>/dev/null |
    tr -d '\r' |
    sed 's/[[:space:]]*$//'
}

version="$(field_value Version "${dir}/DESCRIPTION" || true)"

if [ -z "${version}" ]; then
  echo "::error::No Version field in ${dir}/DESCRIPTION." >&2
  exit 1
fi

emit() {
  printf 'submit=%s\nversion=%s\nreason=%s\n' "$1" "${version}" "$2"
}

components="$(awk -F. '{ print NF }' <<<"${version}")"
if [ "${components}" -gt 3 ]; then
  emit false "${version} is a development version"
  exit 0
fi

if git -C "${dir}" rev-parse -q --verify "refs/tags/v${version}" >/dev/null; then
  emit false "v${version} is already tagged"
  exit 0
fi

recorded_versions() {
  if [ -f "${dir}/CRAN-SUBMISSION" ]; then
    field_value Version "${dir}/CRAN-SUBMISSION"
  fi
  git -C "${dir}" show origin/main:CRAN-SUBMISSION 2>/dev/null |
    sed -n 's/^Version:[[:space:]]*//p' |
    tr -d '\r' |
    sed 's/[[:space:]]*$//' || true
}

if grep -Fxq "${version}" <<<"$(recorded_versions)"; then
  emit false "${version} was already submitted to CRAN"
  exit 0
fi

emit true "${version} is ready to submit"
