#!/usr/bin/env bash
set -euo pipefail

echo "Configuring iTerm2..."

# Check if iTerm is installed
if [ -d "/Applications/iTerm.app" ]; then
    echo "═══════════════════════════════════════════════════════════════"
    echo "                    iTerm2 Setup Instructions                   "
    echo "═══════════════════════════════════════════════════════════════"
    echo ""
    echo "MANUAL STEP REQUIRED:"
    echo ""
    echo "1. Open iTerm2"
    echo "2. Go to: Preferences → Profiles → Other Actions..."
    echo "3. Click: 'Open Dynamic Profiles Folder'"
    echo "4. Verify ~/.config/iterm2/DynamicProfiles/ is present"
    echo "5. Restart iTerm2 if the profile doesn't appear"
    echo ""
    echo "After setup, you'll see 'Dotfiles Default' in your profiles."
    echo ""
    echo "⚠️  WARNING: GUI changes to this profile will be overwritten!"
    echo "═══════════════════════════════════════════════════════════════"
else
    echo "iTerm2 not found. Skipping configuration."
fi