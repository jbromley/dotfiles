function ... --description 'alias ...=cd ../..'
    cd ../..
end

function .... --description 'alias ....=cd ../../..'
    cd ../../..
end

function ..... --description 'alias .....=cd ../../../..'
    cd ../../../..
end

function p --description 'push or pop a directory'
    switch (count $argv)
        case 0
            popd
        case 1
            pushd $argv[1]
        case '*'
            echo "p [DIR]"
    end
end

function zp --description 'push a directory while using zoxide'
    set args (count $argv)
    if test args -eq 1
        set dir (zoxide query $argv[1])
        if test $status -eq 0
            pushd $dir
        end
    else
        echo "Usage: zp DIRECTORY"
    end
end
