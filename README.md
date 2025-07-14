# Aron's Dotfiles

Personal macOS dotfiles managed with [chezmoi](https://www.chezmoi.io/) and integrated with the [1Password CLI](https://developer.1password.com/docs/cli/) for secrets management.

## Installation

This setup is designed to be idempotent—run it on a fresh Mac or an existing one without worry.

### Prerequisites

1. **Xcode Command Line Tools**: Required for Git and other build tools.
   ```bash
   xcode-select --install
   ```
2. **1Password**: Make sure the desktop app is installed and you're signed in. The setup will install the CLI via Homebrew and walk you through authentication.

### One-Liner Setup

Run this in your terminal. It installs chezmoi (if needed), clones this repo, and starts the setup:

```bash
sh -c "$(curl -fsSL get.chezmoi.io)" -- init --apply aronasorman/dotfiles
```

During setup, you'll be prompted for:
- **1Password CLI authentication** (to access your vaults)
- **Your full name** (for Git config)
- **Your email** (for Git config)  
- **Work machine?** (toggles work-specific tools and configs)

## Daily Usage

Keep your dotfiles in sync:
- Pull latest changes: `chezmoi update`
- Edit a managed file: `chezmoi edit ~/.zshrc`
- See what changed: `chezmoi diff`
- Add a new dotfile: `chezmoi add ~/.newconfig`

## Prerequisites

- macOS (Apple Silicon or Intel)
- Command Line Tools: `xcode-select --install`
- 1Password app and CLI (will be installed automatically if missing)

## What's Included

### Shell & Terminal
- Fish shell configuration with starship prompt
- Git configuration with work/personal conditional setup
- SSH config with 1Password SSH agent integration
- Claude AI personal instructions (CLAUDE.md)

### Development Tools
- Homebrew packages and casks
- VS Code extensions
- Various CLI tools (see Brewfile)
- GitHub CLI (gh) configuration and aliases
- Jujutsu (jj) version control configuration
- Global git ignore patterns
- Claude Code CLI (automatically installed via chezmoi if Node.js 18+ is available)

### macOS Defaults
- Finder: Show hidden files, extensions, path bar
- Keyboard: Fast key repeat, disable press-and-hold
- Screenshots: Saved to ~/Pictures/Screenshots

## 1Password Setup

This dotfiles setup uses 1Password for managing secrets. You'll need to:

1. Install 1Password app (automatically installed via Homebrew)
2. Sign in to your 1Password account
3. Enable the 1Password SSH agent:
   - Open 1Password → Settings → Developer
   - Enable "Use the SSH agent"
   - Enable "Integrate with 1Password CLI"

### Adding Secrets to 1Password

For any secrets found in config files (marked with TODO comments), add them to 1Password:

1. Open 1Password
2. Create a new item (e.g., "Secure Note" or "API Credential")
3. Add the secret value
4. Note the reference: `op://vault/item/field`
5. Update the template file to use: `{{ onepasswordRead "op://vault/item/field" }}`

## Customization

### Work vs Personal Setup

During initial setup, you'll be prompted whether this is a work machine. This affects:
- Git configuration (email, signing)
- Shell aliases and functions
- Tool configurations

### Adding New Dotfiles

1. Add the file to your home directory
2. Run `chezmoi add ~/.filename`
3. If it contains secrets, rename to `.tmpl` and add 1Password references
4. Commit and push changes

## Troubleshooting

### 1Password CLI Issues

If you see "1Password CLI is not configured":
```bash
op signin
```

### Homebrew on Apple Silicon

If Homebrew commands aren't found after installation:
```bash
eval "$(/opt/homebrew/bin/brew shellenv)"
```

### SSH Key Issues

Ensure 1Password SSH agent is running:
```bash
ls -la ~/Library/Group\ Containers/2BUA8C4S2C.com.1password/t/agent.sock
```

## Manual Steps

After installation, you may need to:

1. Configure 1Password (sign in, enable SSH agent)
2. Set Fish as default shell: `chsh -s /opt/homebrew/bin/fish`
3. Restart Terminal/iTerm2 for all changes to take effect
4. Install any work-specific tools not in public Brewfile

## Secrets to Migrate

The following secrets were detected and should be moved to 1Password:
- Wakatime API key in `~/.wakatime.cfg` (already configured with 1Password)

## Security Notes

- SSH host IPs are templated and will be prompted during setup (work machines only)
- All SSH configs reference public keys only (private keys managed by 1Password)
- No hardcoded secrets or API keys in the repository

## License

Personal use. Feel free to fork and customize for your own needs.