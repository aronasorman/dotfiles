#!/usr/bin/env bash
set -euo pipefail

directory=$(cd -- "$1" && pwd -P)
tab_title=$2
shift 2

sessions=$(zellij list-sessions --short --no-formatting)
session=
current_session=

while IFS= read -r candidate; do
  [[ -n "$candidate" ]] || continue

  if [[ "$candidate" == "${ZELLIJ_SESSION_NAME:-}" ]]; then
    current_session=$candidate
  fi

  case "/$directory/" in
    *"/$candidate/"*)
      if (( ${#candidate} > ${#session} )); then
        session=$candidate
      fi
      ;;
  esac
done <<< "$sessions"

session="${session:-${current_session:-${sessions%%$'\n'*}}}"

existing_tab_id=$(
  zellij --session "$session" action list-panes --all --json |
    jq -r --arg directory "$directory" --arg tab_title "$tab_title" '
      [
        .[] |
        select(
          .is_plugin == false and
          .exited == false and
          .tab_name == $tab_title and
          .pane_cwd == $directory and
          ((.pane_command // "") | test("(^|/)tuicr( |$)"))
        ) |
        .tab_id
      ] |
      unique |
      first // empty
    '
)

if [[ -n "$existing_tab_id" ]]; then
  printf '%s\t%s\n' "$session" "$existing_tab_id"
  exit 0
fi

tab_id=$(zellij --session "$session" action new-tab \
  --name "$tab_title" \
  --cwd "$directory" \
  --close-on-exit \
  -- tuicr "$@")

printf '%s\t%s\n' "$session" "$tab_id"
