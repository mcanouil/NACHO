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

exit_code_of() {
  local code=0
  "$@" >/dev/null 2>&1 || code=$?
  echo "${code}"
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

make_repo "${tmp}/upstream-crlf" 2.0.7
git clone -q "${tmp}/upstream-crlf" "${tmp}/rerun-crlf"
printf 'Version: 2.0.7\r\nDate: 2026-10-01 10:00:00 UTC\r\nSHA: abc\r\n' \
  >"${tmp}/upstream-crlf/CRAN-SUBMISSION"
git -C "${tmp}/upstream-crlf" add CRAN-SUBMISSION
git -C "${tmp}/upstream-crlf" -c user.name=test -c user.email=test@example.com \
  -c commit.gpgsign=false commit -q -m record
git -C "${tmp}/rerun-crlf" fetch -q origin
out="$("${scripts}/release-guard.sh" "${tmp}/rerun-crlf")"
check "guard skips a CRLF version recorded on origin/main" false \
  "$(field submit <<<"${out}")"

mkdir -p "${tmp}/empty"
check "guard fails without a version" 1 \
  "$(status_of "${scripts}/release-guard.sh" "${tmp}/empty")"

mkdir -p "${tmp}/cran/web/packages/NACHO"
printf 'Package: NACHO\r\nVersion: 2.0.7\r\n' >"${tmp}/cran/web/packages/NACHO/DESCRIPTION"
printf 'Version: 2.0.7\r\nDate: 2026-10-01 10:00:00 UTC\r\nSHA: 0123abc\r\n' \
  >"${tmp}/CRAN-SUBMISSION"
out="$(CRAN_URL="file://${tmp}/cran" \
  "${scripts}/cran-accepted.sh" "${tmp}/CRAN-SUBMISSION")"
check "accepted reports the version" 2.0.7 "$(field version <<<"${out}")"
check "accepted reports the submitted commit" 0123abc \
  "$(field sha <<<"${out}")"

printf 'Package: NACHO\nVersion: 2.0.6\n' >"${tmp}/cran/web/packages/NACHO/DESCRIPTION"
check "accepted stops while CRAN serves the old version" 1 \
  "$(status_of env CRAN_URL="file://${tmp}/cran" \
    "${scripts}/cran-accepted.sh" "${tmp}/CRAN-SUBMISSION")"
check "accepted stops without a submission record" 1 \
  "$(status_of "${scripts}/cran-accepted.sh" "${tmp}/missing")"

printf '* checking CRAN incoming feasibility ... NOTE\nMaintainer: x\n* DONE\n\nStatus: 2 NOTEs\n' \
  >"${tmp}/check-notes.log"
printf '* DONE\n\nStatus: OK\n' >"${tmp}/check-ok.log"
printf '* DONE\n\nStatus: 1 WARNING, 1 NOTE\n' >"${tmp}/check-warn.log"
printf '* checking CRAN incoming feasibility ... NOTE\r\n* DONE\r\n\r\nStatus: 1 NOTE\r\n' \
  >"${tmp}/check-notes-crlf.log"
printf '## R CMD check results\n\n0 errors | 0 warnings | 0 notes\n' \
  >"${tmp}/comments-clean.md"
printf '## R CMD check results\n\n0 errors | 0 warnings | 2 notes\n\n- NOTE one.\n' \
  >"${tmp}/comments-notes.md"
printf '## R CMD check results\r\n\r\n0 errors | 0 warnings | 0 notes\r\n' \
  >"${tmp}/comments-crlf.md"
printf '## R CMD check results\n' >"${tmp}/comments-none.md"

check "results match a clean check" 0 \
  "$(status_of "${scripts}/check-results.sh" "${tmp}/check-ok.log" "${tmp}/comments-clean.md")"
check "results match a CRLF cran-comments.md" 0 \
  "$(status_of "${scripts}/check-results.sh" "${tmp}/check-ok.log" "${tmp}/comments-crlf.md")"
check "results stop on notes cran-comments.md leaves out" 3 \
  "$(exit_code_of "${scripts}/check-results.sh" "${tmp}/check-notes.log" "${tmp}/comments-clean.md")"
check "results match notes cran-comments.md explains" 0 \
  "$(status_of "${scripts}/check-results.sh" "${tmp}/check-notes.log" "${tmp}/comments-notes.md")"
printf '## R CMD check results\n\n0 errors | 0 warnings | 1 note\n\n- NOTE one.\n' \
  >"${tmp}/comments-one-note.md"
check "results accept a note cran-comments.md explains but the check did not find" 0 \
  "$(status_of "${scripts}/check-results.sh" "${tmp}/check-ok.log" "${tmp}/comments-one-note.md")"
check "results stop on a warning" 3 \
  "$(exit_code_of "${scripts}/check-results.sh" "${tmp}/check-warn.log" "${tmp}/comments-notes.md")"
check "results stop without a results line" 1 \
  "$(exit_code_of "${scripts}/check-results.sh" "${tmp}/check-ok.log" "${tmp}/comments-none.md")"
check "results stop without a check log" 1 \
  "$(exit_code_of "${scripts}/check-results.sh" "${tmp}/missing.log" "${tmp}/comments-clean.md")"
out="$("${scripts}/check-results.sh" "${tmp}/check-notes.log" "${tmp}/comments-clean.md" 2>&1 || true)"
check "results name the NOTE section" 1 \
  "$(grep -c 'checking CRAN incoming feasibility' <<<"${out}")"
out="$("${scripts}/check-results.sh" "${tmp}/check-notes-crlf.log" "${tmp}/comments-clean.md" 2>&1 || true)"
check "results name the NOTE section of a CRLF check log" 1 \
  "$(grep -c 'checking CRAN incoming feasibility' <<<"${out}")"

printf '* checking CRAN incoming feasibility ... [3s/54s] NOTE\nMaintainer: x\nDays since last update: 1\n* DONE\n\nStatus: 1 NOTE\n' \
  >"${tmp}/check-update.log"
printf '* checking CRAN incoming feasibility ... NOTE\r\nMaintainer: x\r\n\r\nDays since last update: 3\r\n* DONE\r\n\r\nStatus: 1 NOTE\r\n' \
  >"${tmp}/check-update-crlf.log"
printf '* checking CRAN incoming feasibility ... NOTE\nMaintainer: x\nNew submission\nDays since last update: 1\n* DONE\n\nStatus: 1 NOTE\n' \
  >"${tmp}/check-update-more.log"
printf '* checking CRAN incoming feasibility ... NOTE\nMaintainer: x\nDays since last update: 1\n* checking R code for possible problems ... NOTE\nfoo: no visible binding\n* DONE\n\nStatus: 2 NOTEs\n' \
  >"${tmp}/check-update-other.log"
check "results accept a NOTE that only gives the days since the last update" 0 \
  "$(status_of "${scripts}/check-results.sh" "${tmp}/check-update.log" "${tmp}/comments-clean.md")"
check "results accept that NOTE in a CRLF check log" 0 \
  "$(status_of "${scripts}/check-results.sh" "${tmp}/check-update-crlf.log" "${tmp}/comments-clean.md")"
check "results stop on an incoming NOTE that says more than the days" 3 \
  "$(exit_code_of "${scripts}/check-results.sh" "${tmp}/check-update-more.log" "${tmp}/comments-clean.md")"
check "results stop on another NOTE next to the days since the last update" 3 \
  "$(exit_code_of "${scripts}/check-results.sh" "${tmp}/check-update-other.log" "${tmp}/comments-clean.md")"
check "results do not count the days since the last update as a note to explain" 0 \
  "$(status_of "${scripts}/check-results.sh" "${tmp}/check-update-other.log" "${tmp}/comments-one-note.md")"
out="$("${scripts}/check-results.sh" "${tmp}/check-update-other.log" "${tmp}/comments-clean.md" 2>&1 || true)"
check "results name only the NOTE sections to explain" "0 1" \
  "$(grep -c 'checking CRAN incoming feasibility' <<<"${out}") $(grep -c 'checking R code for possible problems' <<<"${out}")"

if [ "${failures}" -gt 0 ]; then
  printf '%s check(s) failed\n' "${failures}"
  exit 1
fi
echo "All release script checks passed"
