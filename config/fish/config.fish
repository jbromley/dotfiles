if status is-interactive
    # Abbreviations
    abbr --add b bat
    abbr --add bc bc -ql

    abbr --add gadd git add
    abbr --add gco git checkout
    abbr --add gcom git commit
    abbr --add gpull git pull

    abbr --add vimg kitten icat
    abbr --add kfont kitten choose-fonts
    abbr --add ktheme kitten themes

    abbr --add dboxenter distrobox enter
    abbr --add dboxstop distrobox stop

    abbr --add ardcli arduino-cli
    abbr --add psg pgrep -a

    # Commands to run in interactive sessions can go here
    source $HOME/.atuin/bin/env.fish
    atuin init fish | source
    fzf --fish | source
    eval "$(zoxide init fish)"
    mise activate fish | source
    eval "$(starship init fish)"
end
