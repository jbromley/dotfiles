function frg --description 'ripgrep with file preview'
    set -l q $argv
    rg --line-number --no-heading --color=always -- "$q" |
        fzf --ansi --delimiter : --preview '__fzf_preview {1}' \
            --preview-window='right:60%:wrap' \
            --bind 'enter:execute($EDITOR +{2} {1})'
end
