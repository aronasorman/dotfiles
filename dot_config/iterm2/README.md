# iTerm2 Configuration

This directory contains iTerm2 dynamic profiles that are automatically loaded by iTerm2.

## Initial Setup (REQUIRED)

**IMPORTANT**: You must manually enable Dynamic Profiles in iTerm2:

1. Open iTerm2
2. Go to **Preferences → Profiles → Other Actions... → Open Dynamic Profiles Folder**
3. Verify that `~/.config/iterm2/DynamicProfiles/` is being monitored
4. If the file doesn't appear, restart iTerm2

## How it works

iTerm2 automatically monitors `~/.config/iterm2/DynamicProfiles/` for JSON files and loads any profiles defined there. These profiles appear in your profiles list and can be used like any other profile.

**WARNING**: The JSON file is the single source of truth. Any changes made to this profile via the iTerm2 GUI will be **silently overwritten** on the next `chezmoi apply`. This is by design for declarative configuration management.

## Customizing the profile

To export your current iTerm settings:

1. Open iTerm2 → Preferences → Profiles
2. Select your profile
3. Click "Other Actions..." → "Copy Profile as JSON"
4. Edit `DynamicProfiles/dotfiles.json` and update the settings

## Important notes

- The profile generates a unique GUID per installation using `uuidgen`
- Only non-sensitive, shareable settings are included
- Machine-specific settings (like paths with usernames) are avoided
- Font is configurable during setup (defaults to Meslo Nerd Font)
- **IMPORTANT**: The JSON file is the source of truth - GUI changes will be overwritten!

## Configurable settings

During chezmoi setup, you'll be prompted for:
- Font name (default: MesloLGS-NF-Regular)
- Font size (default: 12)

These can be overridden in `~/.config/chezmoi/chezmoi.toml`

## Security considerations

The following iTerm2 settings are **intentionally excluded** from the dynamic profile to prevent information leakage:

### Dangerous Keys (NEVER include these):
- `Command` - Could contain API keys, server names, or secrets
- `Initial Text` / `Send Text at Start` - May contain credentials
- `Custom Command` - Could expose sensitive scripts
- `Triggers` - May contain automatic actions with sensitive data
- `Smart Selection Rules` - Could expose internal patterns

### Other Excluded Settings:
- SSH passwords or keys
- Personal/project paths in Working Directory  
- Machine-specific window arrangements
- Recent directories or commands history
- Badge text with personal information
- Any key containing passwords, tokens, or API keys

## Troubleshooting

- **Profile not appearing**: Restart iTerm2 or check `~/Library/Application Support/iTerm2/DynamicProfiles/`
- **Settings not applying**: Ensure no other profile has the same GUID
- **Font not working**: Run `brew install font-meslo-lg-nerd-font`