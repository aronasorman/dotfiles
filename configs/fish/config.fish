if status is-interactive
    # Commands to run in interactive sessions can go here
end

starship init fish | source
fzf --fish | source
abbr --add nn jj
abbr --add tm task-master
abbr --add claude-yolo claude --dangerously-skip-permissions