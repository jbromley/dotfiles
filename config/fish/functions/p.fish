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
