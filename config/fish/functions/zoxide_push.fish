function zoxide_push --description 'push a directory while using zoxide'
    set args (count $argv)
    if test $args -eq 1
        set dir (zoxide query $argv[1])
        if test $status -eq 0
            pushd $dir
        end
    else
        echo "Usage: zp DIRECTORY"
    end
end
