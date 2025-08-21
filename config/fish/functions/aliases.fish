function ls --description 'alias ls=ls --color=auto --classify'
    command ls --color=auto --classify $argv
end

function grep --description 'alias grep=grep --color=auto'
    command grep --color=auto $argv
end
