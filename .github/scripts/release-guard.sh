#!/usr/bin/env bash
set -euo pipefail

dir="${1:-.}"
version="$(sed -n 's/^Version:[[:space:]]*//p' "${dir}/DESCRIPTION" 2>/dev/null || true)"

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

record="${dir}/CRAN-SUBMISSION"
if [ -f "${record}" ]; then
  recorded="$(sed -n 's/^Version:[[:space:]]*//p' "${record}")"
  if [ "${recorded}" = "${version}" ]; then
    emit false "${version} was already submitted to CRAN"
    exit 0
  fi
fi

emit true "${version} is ready to submit"
