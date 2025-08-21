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

    # fzf preview function
    function __fzf_preview --description 'side-by-side fzf file preview'
        set file $argv[1]
        if test -d "$file"
            command -v tree >/dev/null; and tree -C -L 2 -- "$file" | head -n 200
            return
        end
        bat --color=always --style=plain --paging=never --line-range=:300 -- "$file" 2>/dev/null
    end

    # Commands to run in interactive sessions can go here
    fzf --fish | source
    eval "$(zoxide init fish)"
    mise activate fish | source
    source $HOME/.atuin/bin/env.fish
    atuin init fish | source
    eval "$(starship init fish)"
end
