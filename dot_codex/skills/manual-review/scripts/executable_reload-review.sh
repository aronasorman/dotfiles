#!/usr/bin/env bash
set -euo pipefail

if (( $# != 4 )); then
  printf 'usage: %s <repository-path> <tab-title> <session> <tab-id>\n' "$0" >&2
  exit 2
fi

directory=$(cd -- "$1" && pwd -P)
tab_title=$2
session=$3
tab_id=$4

if [[ ! "$tab_id" =~ ^[0-9]+$ ]]; then
  printf 'reload-review: tab ID must be numeric: %s\n' "$tab_id" >&2
  exit 2
fi

panes=$(zellij --session "$session" action list-panes --all --json)
matches=$(
  jq -c \
    --arg directory "$directory" \
    --arg tab_title "$tab_title" \
    --argjson tab_id "$tab_id" '
      [
        .[] |
        select(
          .is_plugin == false and
          .exited == false and
          .tab_id == $tab_id and
          .tab_name == $tab_title and
          .pane_cwd == $directory and
          ((.pane_command // .terminal_command // "") | test("(^|/)tuicr( |$)"))
        )
      ]
    ' <<<"$panes"
)
match_count=$(jq 'length' <<<"$matches")

if (( match_count != 1 )); then
  printf \
    'reload-review: expected exactly one live tuicr pane for session %s, tab %s, title %q, and path %q; found %s\n' \
    "$session" "$tab_id" "$tab_title" "$directory" "$match_count" >&2
  exit 1
fi

pane_id=$(jq -r '.[0].id' <<<"$matches")
zellij --session "$session" action write-chars --pane-id "$pane_id" ':e'
zellij --session "$session" action write --pane-id "$pane_id" 13

printf '%s\t%s\t%s\n' "$session" "$tab_id" "$pane_id"
