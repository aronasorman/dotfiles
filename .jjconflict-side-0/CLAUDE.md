# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a personal dotfiles repository managed with Chezmoi, designed for macOS systems. It uses 1Password for secure secret management and includes configurations for development tools, shell environments, and applications.

## Commands

### Dotfile Management
- Apply changes: `chezmoi apply`
- Update from repo: `chezmoi update`
- Edit managed file: `chezmoi edit ~/.config/fish/config.fish`
- Add new dotfile: `chezmoi add ~/.newconfig`
- See pending changes: `chezmoi diff`
- Test templates: `chezmoi execute-template < file.tmpl`

### Development
- Install packages: `brew bundle`
- Update packages: `brew update && brew upgrade`

## Architecture

### Chezmoi Structure
- Files prefixed with `dot_` become dotfiles (e.g., `dot_gitconfig` → `~/.gitconfig`)
- `private_` prefix sets restricted permissions
- `.tmpl` suffix indicates Go template files that process variables and secrets
- `dot_config/` maps to `~/.config/`
- `.chezmoi.toml.tmpl` contains prompts and variables used during setup

### Secret Management
- All secrets are stored in 1Password and accessed via `{{ onepasswordRead "op://vault/item/field" }}`
- The `.install-password-manager.sh` hook ensures 1Password CLI is available before Chezmoi runs
- Never commit secrets directly - always use 1Password references in template files

### Key Components
- **Shell**: Fish shell with minimal config, using Starship prompt and fzf integration
- **Git**: Conditional configuration for work/personal emails
- **SSH**: Uses 1Password SSH agent for all key management
- **Package Management**: Brewfile contains all tools, apps, and dependencies

### Work vs Personal Setup
The setup distinguishes between work and personal machines via the `work` variable in `.chezmoi.toml`. Work machines include:
- Additional SSH host configurations with templated IPs
- Work-specific Git signing configuration
- Enterprise tool installations

When modifying configurations, ensure changes work for both environments by using conditional templating where appropriate.