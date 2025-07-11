#!/usr/bin/env bash
set -euo pipefail

if ! command -v brew >/dev/null 2>&1; then
  echo "Installing Homebrew..."
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
  
  if [[ $(uname -m) == "arm64" ]]; then
    echo "Configuring Homebrew for Apple Silicon..."
    eval "$(/opt/homebrew/bin/brew shellenv)"
  else
    echo "Configuring Homebrew for Intel..."
    eval "$(/usr/local/bin/brew shellenv)"
  fi
else
  echo "Homebrew already installed."
fi