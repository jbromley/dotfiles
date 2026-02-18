if status is-interactive
    # Commands to run in interactive sessions can go here
    set --global EDITOR hx
    set --global hydro_multiline true
    dprint completions fish | source
    fzf --fish | source
    eval "$(zoxide init fish)"
    mise activate fish | source
    eval (starship init fish)
end
