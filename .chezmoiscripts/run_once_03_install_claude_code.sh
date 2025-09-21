#!/usr/bin/env bash
set -euo pipefail

echo "Checking Claude Code CLI installation..."

# Check if Node.js is installed
if ! command -v node &> /dev/null; then
    echo "Warning: Node.js is not installed. Skipping Claude Code installation."
    echo "Install Node.js 18+ via Homebrew (brew install node) to use Claude Code."
    exit 0
fi

# Check Node.js version
NODE_VERSION=$(node -v | cut -d'v' -f2 | cut -d'.' -f1)
if [ "$NODE_VERSION" -lt 18 ]; then
    echo "Warning: Node.js 18+ required for Claude Code. Current version: $(node -v)"
    echo "Update Node.js to use Claude Code."
    exit 0
fi

# Check if Claude Code is already installed
if command -v claude &> /dev/null; then
    INSTALLED_VERSION=$(claude --version 2>/dev/null || echo "unknown")
    echo "Claude Code is already installed (version: $INSTALLED_VERSION)"
    exit 0
fi

# Install Claude Code
echo "Installing Claude Code CLI..."
if npm install -g @anthropic-ai/claude-code; then
    echo "✓ Claude Code installed successfully!"
    echo "Run 'claude' in any project directory to start."
else
    echo "Failed to install Claude Code. You can install it manually with:"
    echo "npm install -g @anthropic-ai/claude-code"
    exit 0
fi