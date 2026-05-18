#!/usr/bin/env bash
set -euo pipefail

count="${1:-1}"
host="${FORGEJO_REVIEW_HOST:-host.gorgon-ratio.ts.net}"
counter="${FORGEJO_RV_COUNTER:-/home/aron/forgejo/rv-counter.txt}"
lock="${FORGEJO_RV_LOCK:-/home/aron/forgejo/rv-counter.lock}"

case "$count" in
  ''|*[!0-9]*)
    echo "usage: $0 [positive-count]" >&2
    exit 2
    ;;
esac

if [ "$count" -lt 1 ]; then
  echo "usage: $0 [positive-count]" >&2
  exit 2
fi

ssh "$host" "set -euo pipefail
counter='$counter'
lock='$lock'
count='$count'
exec 9>\"\$lock\"
flock 9
n=\$(cat \"\$counter\")
i=0
while [ \"\$i\" -lt \"\$count\" ]; do
  printf 'rv-%04d\n' \$((n + i))
  i=\$((i + 1))
done
printf '%s\n' \$((n + count)) > \"\$counter\"
"
