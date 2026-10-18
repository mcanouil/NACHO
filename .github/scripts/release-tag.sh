#!/usr/bin/env bash
set -euo pipefail

# Tell whether the tag v<version> exists, and stop when it points to another
# commit than the one submitted to CRAN.
# Usage: release-tag.sh <version> <sha> [dir]

version="${1:-}"
sha="${2:-}"
dir="${3:-.}"

if [ -z "${version}" ] || [ -z "${sha}" ]; then
  echo "::error::Usage: release-tag.sh <version> <sha> [dir]" >&2
  exit 1
fi

tagged="$(git -C "${dir}" rev-parse -q --verify "refs/tags/v${version}^{commit}" || true)"
if [ -z "${tagged}" ]; then
  echo "tagged=false"
  exit 0
fi

submitted="$(git -C "${dir}" rev-parse --verify "${sha}^{commit}")"
if [ "${tagged}" != "${submitted}" ]; then
  echo "::error::v${version} points to ${tagged}, not to the submitted commit ${submitted}. Check the tag by hand before you run this again." >&2
  exit 1
fi

echo "tagged=true"
