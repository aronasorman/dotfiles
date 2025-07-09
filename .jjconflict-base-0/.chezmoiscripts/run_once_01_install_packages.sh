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

# Set Fish as default shell if it's not already
if [[ "$SHELL" != *"fish"* ]]; then
    echo "Setting Fish as default shell..."
    
    # Get the path to fish
    if [[ $(uname -m) == "arm64" ]]; then
        FISH_PATH="/opt/homebrew/bin/fish"
    else
        FISH_PATH="/usr/local/bin/fish"
    fi
    
    # Add fish to /etc/shells if not already there
    if ! grep -q "$FISH_PATH" /etc/shells; then
        echo "Adding $FISH_PATH to /etc/shells..."
        echo "$FISH_PATH" | sudo tee -a /etc/shells
    fi
    
    # Change default shell to fish
    chsh -s "$FISH_PATH"
    echo "Fish shell is now your default shell. Please restart your terminal."
else
    echo "Fish is already your default shell."
fi