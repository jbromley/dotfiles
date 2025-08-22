if status is-interactive
    fish_add_path --path $HOME/.local/bin

    # Commands to run in interactive sessions can go here
    fzf --fish | source
    eval "$(zoxide init fish)"
    mise activate fish | source
    source $HOME/.atuin/bin/env.fish
    atuin init fish | source
end
