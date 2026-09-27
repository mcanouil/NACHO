#!/usr/bin/env bash
set -euo pipefail

usage="Usage: bump-version.sh <major|minor|patch|dev> [dir]"
which="${1:?${usage}}"
dir="${2:-.}"
desc="${dir}/DESCRIPTION"
news="${dir}/NEWS.md"

case "${which}" in
major) level=1 ;;
minor) level=2 ;;
patch) level=3 ;;
dev) level=4 ;;
*)
  echo "::error::Unknown bump '${which}'. ${usage}" >&2
  exit 1
  ;;
esac

if [ ! -f "${desc}" ]; then
  echo "::error::No ${desc} found." >&2
  exit 1
fi

package="$(sed -n 's/^Package:[[:space:]]*//p' "${desc}" | tr -d '[:space:]')"
version="$(sed -n 's/^Version:[[:space:]]*//p' "${desc}" | tr -d '[:space:]')"

if ! printf '%s\n' "${version}" | grep -Eq '^[0-9]+(\.[0-9]+){1,3}$'; then
  echo "::error::Version '${version}' in ${desc} is not of the form X.Y, X.Y.Z or X.Y.Z.W." >&2
  exit 1
fi

components="$(awk -F. '{ print NF }' <<<"${version}")"
if [ "${which}" = "dev" ] && [ "${components}" -gt 3 ]; then
  printf 'version=%s\nprevious=%s\n' "${version}" "${version}"
  exit 0
fi

new="$(
  awk -v v="${version}" -v w="${level}" 'BEGIN {
    n = split(v, c, ".")
    inc = (w == 4 && n < 4) ? 9000 : 1
    for (i = n + 1; i <= w; i++) c[i] = 0
    if (w > n) n = w
    c[w] = c[w] + inc
    for (i = w + 1; i <= n; i++) c[i] = 0
    if (n > 3) {
      zero = 1
      for (i = 4; i <= n; i++) if (c[i] + 0 != 0) zero = 0
      if (zero) n = 3
    }
    out = c[1] + 0
    for (i = 2; i <= n; i++) out = out "." (c[i] + 0)
    print out
  }'
)"

tmp="$(mktemp)"
trap 'rm -f "${tmp}"' EXIT

awk -v new="${new}" '
  /^Version:/ && !done { print "Version: " new; done = 1; next }
  { print }
' "${desc}" >"${tmp}"
cat "${tmp}" >"${desc}"

if [ -f "${news}" ]; then
  if [ "${which}" = "dev" ]; then
    title="# ${package} (development version)"
  else
    title="# ${package} ${new}"
  fi
  development="# ${package} (development version)"
  first_line="$(awk '/[^[:space:]]/ { print NR; exit }' "${news}")"
  if [ -n "${first_line}" ]; then
    first="$(sed -n "${first_line}p" "${news}")"
    if [ "${first}" = "${development}" ] && [ "${first}" != "${title}" ]; then
      awk -v line="${first_line}" -v title="${title}" '
        NR == line { print title; next }
        { print }
      ' "${news}" >"${tmp}"
      cat "${tmp}" >"${news}"
    elif [ "${first}" != "${title}" ]; then
      {
        printf '%s\n\n' "${title}"
        cat "${news}"
      } >"${tmp}"
      cat "${tmp}" >"${news}"
    fi
  fi
fi

printf 'version=%s\nprevious=%s\n' "${new}" "${version}"
