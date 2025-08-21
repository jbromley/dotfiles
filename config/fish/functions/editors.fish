function h --description 'open a file in Helix, choose with fzf'
    set args (count $argv)
    if test $args -eq 0
        set f (fzf)
        if test $status -eq 0
            hx $f
        end
    else
        hx $argv[1]
    end
end

function e --description 'open a file in Emacs, choose with fzf'
    set args (count $argv)
    if test $args -eq 0
        set f (fzf)
        if test $status -eq 0
            emacs $f
        end
    else
        emacs $argv[1]
    end
end
