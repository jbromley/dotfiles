function precmd {
}

function set_running_app {
    printf "\e]1; $PWD:t:$(history $HISTCMD | cut -b7- ) \a"
}

function preexec {
}

function postexec {
}
