#!/usr/bin/env bash
set -euo pipefail

base="${1:-main}"

usage() {
  cat >&2 <<'EOF'
usage: strip-review.sh <base-branch>

Rewrites commits in <base-branch>..HEAD by removing commit-message content
from a line that is exactly "-----" through the end of the message.

Run only on a dedicated Forgejo review branch with a clean worktree.
EOF
}

if [ "${1:-}" = "-h" ] || [ "${1:-}" = "--help" ]; then
  usage
  exit 0
fi

git rev-parse --is-inside-work-tree >/dev/null
git rev-parse --verify "$base^{commit}" >/dev/null

git update-index -q --refresh
if ! git diff --quiet || ! git diff --cached --quiet; then
  echo "strip-review.sh requires a clean working tree and index" >&2
  exit 1
fi

commit_count=$(git rev-list --count "$base..HEAD")
if [ "$commit_count" -eq 0 ]; then
  echo "No commits to rewrite in $base..HEAD"
  exit 0
fi

if ! git log --format='%B%n' "$base..HEAD" | grep -q '^-----$'; then
  echo "No REVIEW separator found in $base..HEAD"
  exit 0
fi

export FILTER_BRANCH_SQUELCH_WARNING=1
git filter-branch -f --msg-filter '
awk "
  /^-----$/ { drop = 1; next }
  !drop { print }
"
' "$base..HEAD"

while IFS= read -r ref; do
  [ -n "$ref" ] || continue
  git update-ref -d "$ref"
done <<EOF
$(git for-each-ref --format="%(refname)" refs/original/)
EOF

remaining=$(git log --format='%s%n%b' "$base..HEAD" | grep -c '^REVIEW$' || true)
if [ "$remaining" != "0" ]; then
  echo "Review metadata may remain: $remaining REVIEW markers found" >&2
  exit 1
fi

echo "Stripped REVIEW sections from $commit_count commit(s) in $base..HEAD"
