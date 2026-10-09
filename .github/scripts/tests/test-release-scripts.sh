#!/usr/bin/env bash
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
scripts="$(cd "${here}/.." && pwd)"
tmp="$(mktemp -d)"
trap 'rm -rf "${tmp}"' EXIT
failures=0

check() {
  if [ "$2" = "$3" ]; then
    printf 'ok   %s\n' "$1"
  else
    printf 'FAIL %s\n  expected: %s\n  actual:   %s\n' "$1" "$2" "$3"
    failures=$((failures + 1))
  fi
}

field() {
  sed -n "s/^$1=//p"
}

make_repo() {
  mkdir -p "$1"
  printf 'Package: NACHO\nVersion: %s\n' "$2" >"$1/DESCRIPTION"
  git -C "$1" init -q
  git -C "$1" add DESCRIPTION
  git -C "$1" -c user.name=test -c user.email=test@example.com \
    -c commit.gpgsign=false commit -q -m init
}

status_of() {
  if "$@" >/dev/null 2>&1; then echo 0; else echo 1; fi
}

make_repo "${tmp}/dev" 2.0.7.9000
out="$("${scripts}/release-guard.sh" "${tmp}/dev")"
check "guard skips a development version" false "$(field submit <<<"${out}")"

make_repo "${tmp}/ready" 2.0.7
out="$("${scripts}/release-guard.sh" "${tmp}/ready")"
check "guard submits a release version" true "$(field submit <<<"${out}")"
check "guard reports the version" 2.0.7 "$(field version <<<"${out}")"

git -C "${tmp}/ready" -c tag.gpgSign=false tag v2.0.7
out="$("${scripts}/release-guard.sh" "${tmp}/ready")"
check "guard skips a tagged version" false "$(field submit <<<"${out}")"

make_repo "${tmp}/submitted" 2.0.7
printf 'Version: 2.0.7\nDate: 2026-10-01 10:00:00 UTC\nSHA: abc\n' \
  >"${tmp}/submitted/CRAN-SUBMISSION"
out="$("${scripts}/release-guard.sh" "${tmp}/submitted")"
check "guard skips an already submitted version" false \
  "$(field submit <<<"${out}")"

make_repo "${tmp}/upstream" 2.0.7
git clone -q "${tmp}/upstream" "${tmp}/rerun"
printf 'Version: 2.0.7\nDate: 2026-10-01 10:00:00 UTC\nSHA: abc\n' \
  >"${tmp}/upstream/CRAN-SUBMISSION"
git -C "${tmp}/upstream" add CRAN-SUBMISSION
git -C "${tmp}/upstream" -c user.name=test -c user.email=test@example.com \
  -c commit.gpgsign=false commit -q -m record
git -C "${tmp}/rerun" fetch -q origin
out="$("${scripts}/release-guard.sh" "${tmp}/rerun")"
check "guard skips a version recorded on origin/main" false \
  "$(field submit <<<"${out}")"

make_repo "${tmp}/crlf" 2.0.7
printf 'Package: NACHO\r\nVersion: 2.0.7 \r\n' >"${tmp}/crlf/DESCRIPTION"
out="$("${scripts}/release-guard.sh" "${tmp}/crlf")"
check "guard reads a version with CRLF line ends" 2.0.7 \
  "$(field version <<<"${out}")"
check "guard submits a CRLF release version" true \
  "$(field submit <<<"${out}")"

printf 'Version: 2.0.7\r\nDate: 2026-10-01 10:00:00 UTC\r\nSHA: abc\r\n' \
  >"${tmp}/crlf/CRAN-SUBMISSION"
out="$("${scripts}/release-guard.sh" "${tmp}/crlf")"
check "guard skips a CRLF submission record" false \
  "$(field submit <<<"${out}")"

mkdir -p "${tmp}/empty"
check "guard fails without a version" 1 \
  "$(status_of "${scripts}/release-guard.sh" "${tmp}/empty")"

cat >"${tmp}/NEWS.md" <<'EOF'
# NACHO 2.0.7

## Bug fixes

- fix: Compute PCA scores for samples. (#60)

# NACHO 2.0.6

- fix: Older entry.
EOF

out="$("${scripts}/news-section.sh" 2.0.7 "${tmp}/NEWS.md")"
check "news section starts at its first line" "## Bug fixes" \
  "$(head -n 1 <<<"${out}")"
check "news section stops before the next version" 0 \
  "$(grep -c 'Older entry' <<<"${out}" || true)"
check "news section fails for a missing version" 1 \
  "$(status_of "${scripts}/news-section.sh" 9.9.9 "${tmp}/NEWS.md")"
out="$("${scripts}/news-section.sh" 2.0.6 - <"${tmp}/NEWS.md")"
check "news section reads standard input" "- fix: Older entry." "${out}"

mkdir -p "${tmp}/crandb"
printf '{"Package":"NACHO","Version":"2.0.7"}\n' >"${tmp}/crandb/NACHO"
printf 'Version: 2.0.7\nDate: 2026-10-01 10:00:00 UTC\nSHA: 0123abc\n' \
  >"${tmp}/CRAN-SUBMISSION"
out="$(CRANDB_URL="file://${tmp}/crandb" \
  "${scripts}/cran-accepted.sh" "${tmp}/CRAN-SUBMISSION")"
check "accepted reports the version" 2.0.7 "$(field version <<<"${out}")"
check "accepted reports the submitted commit" 0123abc \
  "$(field sha <<<"${out}")"

printf '{"Package":"NACHO","Version":"2.0.6"}\n' >"${tmp}/crandb/NACHO"
check "accepted stops while CRAN serves the old version" 1 \
  "$(status_of env CRANDB_URL="file://${tmp}/crandb" \
    "${scripts}/cran-accepted.sh" "${tmp}/CRAN-SUBMISSION")"
check "accepted stops without a submission record" 1 \
  "$(status_of "${scripts}/cran-accepted.sh" "${tmp}/missing")"

make_pkg() {
  mkdir -p "$1"
  printf 'Package: NACHO\nTitle: NanoString QC\nVersion: %s\nLicense: GPL-3\n' \
    "$2" >"$1/DESCRIPTION"
  printf '%b' "$3" >"$1/NEWS.md"
}

version_of() {
  sed -n 's/^Version:[[:space:]]*//p' "$1/DESCRIPTION"
}

make_pkg "${tmp}/bump-patch" 2.0.6.9000 \
  '# NACHO (development version)\n\n- fix: Something. (#60)\n\n# NACHO 2.0.6\n\n- fix: Older.\n'
out="$("${scripts}/bump-version.sh" patch "${tmp}/bump-patch")"
check "bump patch from a development version" 2.0.7 "$(field version <<<"${out}")"
check "bump reports the previous version" 2.0.6.9000 "$(field previous <<<"${out}")"
check "bump writes DESCRIPTION" 2.0.7 "$(version_of "${tmp}/bump-patch")"
check "bump keeps the other DESCRIPTION lines" "Title: NanoString QC" \
  "$(sed -n 2p "${tmp}/bump-patch/DESCRIPTION")"
check "bump keeps the DESCRIPTION line count" 4 \
  "$(wc -l <"${tmp}/bump-patch/DESCRIPTION" | tr -d ' ')"
check "release renames the development heading" "# NACHO 2.0.7" \
  "$(sed -n 1p "${tmp}/bump-patch/NEWS.md")"
check "release keeps the NEWS body" "- fix: Something. (#60)" \
  "$(sed -n 3p "${tmp}/bump-patch/NEWS.md")"
check "release keeps the NEWS line count" 7 \
  "$(wc -l <"${tmp}/bump-patch/NEWS.md" | tr -d ' ')"

make_pkg "${tmp}/bump-major" 2.0.7.9000 '# NACHO (development version)\n'
out="$("${scripts}/bump-version.sh" major "${tmp}/bump-major")"
check "bump major from a development version" 3.0.0 "$(field version <<<"${out}")"

make_pkg "${tmp}/bump-minor" 2.0.7 '# NACHO 2.0.7\n\n- fix: Older.\n'
out="$("${scripts}/bump-version.sh" minor "${tmp}/bump-minor")"
check "bump minor from a release" 2.1.0 "$(field version <<<"${out}")"
check "release adds a heading when none is pending" "# NACHO 2.1.0" \
  "$(sed -n 1p "${tmp}/bump-minor/NEWS.md")"
check "release leaves a blank line under the new heading" "" \
  "$(sed -n 2p "${tmp}/bump-minor/NEWS.md")"
check "release keeps the old heading below" "# NACHO 2.0.7" \
  "$(sed -n 3p "${tmp}/bump-minor/NEWS.md")"

make_pkg "${tmp}/bump-dev" 2.0.7 '# NACHO 2.0.7\n\n- fix: Older.\n'
out="$("${scripts}/bump-version.sh" dev "${tmp}/bump-dev")"
check "dev bump appends 9000" 2.0.7.9000 "$(field version <<<"${out}")"
check "dev bump adds the development heading" "# NACHO (development version)" \
  "$(sed -n 1p "${tmp}/bump-dev/NEWS.md")"
check "dev bump leaves a blank line under it" "" \
  "$(sed -n 2p "${tmp}/bump-dev/NEWS.md")"
check "dev bump keeps the release heading below" "# NACHO 2.0.7" \
  "$(sed -n 3p "${tmp}/bump-dev/NEWS.md")"

cp "${tmp}/bump-dev/NEWS.md" "${tmp}/news-before.md"
out="$("${scripts}/bump-version.sh" dev "${tmp}/bump-dev")"
check "dev bump on a development version changes nothing" 2.0.7.9000 \
  "$(version_of "${tmp}/bump-dev")"
check "dev bump on a development version keeps NEWS" "" \
  "$(diff "${tmp}/news-before.md" "${tmp}/bump-dev/NEWS.md" || true)"

make_pkg "${tmp}/bump-same" 2.0.6.9000 '# NACHO 2.0.7\n\n- fix: Written early.\n'
"${scripts}/bump-version.sh" patch "${tmp}/bump-same" >/dev/null
check "release keeps a heading that already matches" 1 \
  "$(grep -c '^# NACHO 2.0.7$' "${tmp}/bump-same/NEWS.md")"

make_pkg "${tmp}/bump-blank" 2.0.6.9000 '\n# NACHO (development version)\n\n- fix: X.\n'
"${scripts}/bump-version.sh" patch "${tmp}/bump-blank" >/dev/null
check "release renames a heading after leading blank lines" "# NACHO 2.0.7" \
  "$(sed -n 2p "${tmp}/bump-blank/NEWS.md")"

make_pkg "${tmp}/bump-bad" 2.0.7 '# NACHO 2.0.7\n'
check "bump rejects an unknown level" 1 \
  "$(status_of "${scripts}/bump-version.sh" huge "${tmp}/bump-bad")"
check "a rejected bump leaves DESCRIPTION alone" 2.0.7 \
  "$(version_of "${tmp}/bump-bad")"
make_pkg "${tmp}/bump-malformed" 2.0.x '# NACHO 2.0.x\n'
check "bump rejects a malformed version" 1 \
  "$(status_of "${scripts}/bump-version.sh" patch "${tmp}/bump-malformed")"
check "bump fails without DESCRIPTION" 1 \
  "$(status_of "${scripts}/bump-version.sh" patch "${tmp}/missing")"

if [ "${failures}" -gt 0 ]; then
  printf '%s check(s) failed\n' "${failures}"
  exit 1
fi
echo "All release script checks passed"
