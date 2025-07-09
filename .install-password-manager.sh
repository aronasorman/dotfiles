#!/usr/bin/env bash
set -euo pipefail

# Install 1Password CLI if not present
if ! command -v op >/dev/null 2>&1; then
    echo "1Password CLI not found. Installing..."
    
    if command -v brew >/dev/null 2>&1; then
        brew install --cask 1password-cli
    else
        echo "Error: Homebrew not found. Please install 1Password CLI manually."
        echo "Visit: https://developer.1password.com/docs/cli/get-started/"
        exit 1
    fi
fi

# Check if 1Password is configured
if ! op account list >/dev/null 2>&1; then
    echo "1Password CLI is not configured. Please sign in:"
    echo "Run: op signin"
    exit 1
fi

echo "1Password CLI is installed and configured."