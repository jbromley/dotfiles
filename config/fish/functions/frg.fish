function frg --description 'ripgrep with fzf file preview'
    set -l rg_args $argv[1..-2]
    set -l q $argv[-1]
    rg --line-number --no-heading --color=always $rg_args -- "$q" |
        fzf --ansi --delimiter : --height=20 \
            --preview 'bat --color=always --highlight-line {2} -- {1}' \
            --preview-window='right:60%:wrap:+{2}-5' \
            --bind 'enter:become($EDITOR +{2} {1})'
end
