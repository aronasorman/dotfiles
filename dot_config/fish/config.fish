# Only execute this file once per shell.
set -q __fish_config_sourced; and exit
set -g __fish_config_sourced 1

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

    if test "$TERM" != dumb
        starship init fish | source
    end

end
