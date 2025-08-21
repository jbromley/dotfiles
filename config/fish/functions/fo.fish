function fo --description 'fuzzy-open a file with live preview'
    set -l f (fzf)
    test -n "$f"; and $EDITOR "$f"
end
