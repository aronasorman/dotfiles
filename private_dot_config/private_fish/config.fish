# PATH modifications

set -x EDITOR "code -w"
set -x SHELL /usr/local/bin/fish
set -x GOPATH /Users/aron
set -x PIPENV_MAX_DEPTH 6
set -gx HOMEBREW_GITHUB_API_TOKEN 5c22e8314949cb47547ae829fe1f93c8f1eb1466

# activate nix-darwin

# use virtualenv-fish
# eval (/usr/local/bin/python3 -m virtualfish)

# add ~/bin to PATH
set fish_user_paths ~/bin/ ~/.fzf/bin 
source ~/.asdf/asdf.fish

# set fzf for ctrl+r
fzf_key_bindings


## emacs shell compatibility
if test -n "$EMACS"
    set -x TERM eterm-color
end

# this function may be required
function fish_title
    true
end
