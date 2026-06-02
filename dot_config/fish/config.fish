# Only execute this file once per shell.
set -q __fish_config_sourced; and exit
set -g __fish_config_sourced 1

# Keep Swamp/Kubernetes fan-out workflows from hitting macOS' low default fd limit.
ulimit -S -n 8192 2>/dev/null; or true

fish_add_path /opt/homebrew/bin
fish_add_path /Users/aron/.antigravity/antigravity/bin

status is-login; and begin

    # Login shell initialisation

end

status is-interactive; and begin

    # Abbreviations
    abbr --add -- claude-yolo 'claude --dangerously-skip-permissions'
    abbr --add -- nn jj
    abbr --add -- tm task-master

    # Aliases

    # Interactive shell initialisation
    fzf --fish | source
    mise activate fish | source

    if test "$TERM" != dumb
        starship init fish | source
    end

end

# Added by Antigravity
fish_add_path /Users/aron/.antigravity/antigravity/bin
set -gx PATH /Users/aron/.local/bin $PATH
