#!/usr/bin/env bash
set -euo pipefail

record="${1:-CRAN-SUBMISSION}"
package="${2:-NACHO}"
crandb="${CRANDB_URL:-https://crandb.r-pkg.org}"

if [ ! -f "${record}" ]; then
  echo "::error::No ${record} found. Nothing is waiting for CRAN, or the release is already published." >&2
  exit 1
fi

version="$(sed -n 's/^Version:[[:space:]]*//p' "${record}")"
sha="$(sed -n 's/^SHA:[[:space:]]*//p' "${record}")"
if [ -z "${version}" ] || [ -z "${sha}" ]; then
  echo "::error::${record} lacks a Version or SHA field." >&2
  exit 1
fi

served="$(curl -fsSL "${crandb}/${package}" | jq -r '.Version')"
if [ "${served}" != "${version}" ]; then
  echo "::error::CRAN serves ${package} ${served}, not ${version}. Run this again once CRAN's acceptance email arrives." >&2
  exit 1
fi

printf 'version=%s\nsha=%s\n' "${version}" "${sha}"
