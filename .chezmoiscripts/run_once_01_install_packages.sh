#!/usr/bin/env bash
set -euo pipefail

echo "Installing packages from Brewfile..."

# Ensure Homebrew is in PATH
if [[ $(uname -m) == "arm64" ]]; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
else
    eval "$(/usr/local/bin/brew shellenv)"
fi

brew bundle --file="{{ .chezmoi.sourceDir }}/Brewfile"
brew autoremove
brew cleanup --prune=all