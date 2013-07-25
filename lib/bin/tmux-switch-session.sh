#!/usr/bin/env bash
set -e

if [[ -z $1 ]]; then
  tmux switch-client -l
else
  tmux switch-client -t "$1"
fi
