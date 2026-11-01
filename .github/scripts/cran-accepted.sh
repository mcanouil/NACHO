#!/usr/bin/env bash
set -euo pipefail

record="${1:-CRAN-SUBMISSION}"
package="${2:-NACHO}"
cran="${CRAN_URL:-https://cran.r-project.org}"

if [ ! -f "${record}" ]; then
  echo "::error::No ${record} found. Nothing is waiting for CRAN, or the release is already published." >&2
  exit 1
fi

field() {
  sed -n "s/^$1:[[:space:]]*//p" | sed 's/[[:space:]]*$//'
}

version="$(field Version <"${record}")"
sha="$(field SHA <"${record}")"
if [ -z "${version}" ] || [ -z "${sha}" ]; then
  echo "::error::${record} lacks a Version or SHA field." >&2
  exit 1
fi

served="$(curl -fsSL "${cran}/web/packages/${package}/DESCRIPTION" | field Version)"
if [ "${served}" != "${version}" ]; then
  echo "::error::CRAN serves ${package} ${served}, not ${version}. Run this again once CRAN's acceptance email arrives." >&2
  exit 1
fi

printf 'version=%s\nsha=%s\n' "${version}" "${sha}"
