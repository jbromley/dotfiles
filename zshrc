# ~/.zshrc: executed by zsh(1) for interactive shells.
# Environment variables live in ~/.zshenv instead, since that file is
# read for every zsh invocation, not just interactive ones.

# If not running interactively, don't do anything.
[[ -o interactive ]] || return

# History behavior (HISTFILE/HISTSIZE/SAVEHIST are set in ~/.zshenv).
setopt HIST_IGNORE_SPACE     # Don't record lines that start with a space.
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS  # Drop older duplicates when a new one is recorded.
setopt HIST_SAVE_NO_DUPS     # Don't write duplicate lines to HISTFILE.
setopt HIST_REDUCE_BLANKS    # Trim superfluous whitespace before recording.
setopt SHARE_HISTORY         # Append immediately and pick up other sessions' history.

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# Enable color support of ls/grep (aliases live in ~/.aliases) via dircolors.
if command -v dircolors >/dev/null 2>&1; then
    if [ -r "${HOME}/.dircolors" ]; then
        eval "$(dircolors -b "${HOME}/.dircolors")"
    else
        eval "$(dircolors -b)"
    fi
fi

# colored GCC warnings and errors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'
export COLORTERM=truecolor

# Add an "alert" alias for long running commands. Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(fc -ln -1 | sed -e '\''s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.aliases, instead of adding them here directly.
if [ -f "${HOME}/.aliases" ]; then
    source "${HOME}/.aliases"
fi

# Functions
p() {
    case $# in
        0)
             popd || exit
            ;;
        1)
            pushd "$1" || exit
            ;;
        *)
            print -u2 "p [DIR]"
            return 1
            ;;
    esac
}

zp() {
    if (( $# != 1 )); then
        print -u2 "Usage: zp DIRECTORY"
        return 1
    fi

    local dir
    if dir="$(zoxide query "$1")"; then
        pushd "$dir" >/dev/null || exit
    fi
}

# Completion
: "${XDG_CACHE_HOME:=${HOME}/.cache}"
fpath=("${HOME}/.zsh" ${fpath})
[ -r "${HOME}/.zsh/completion.zsh" ] && source "${HOME}/.zsh/completion.zsh"
autoload -Uz compinit
compinit

# Set up local bin directory.
if [ -d "$HOME/.local/bin" ] ; then
    path+=${HOME}/.local/bin
fi

eval "$(mise activate zsh)"
eval "$(dprint completions zsh)"
eval "$(fzf --zsh)"
eval "$(starship init zsh)"
eval "$(zoxide init zsh)"

# Syntax highlighting. Sourced last, since it needs to wrap widgets defined
# by everything above. Not bundled with zsh itself, so this looks for either
# a manually cloned copy of https://github.com/zsh-users/zsh-syntax-highlighting
# under ~/.zsh or a distro package providing it.
for highlighting_script in \
    "${HOME}/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh" \
    /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh \
    /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
do
    if [ -r "${highlighting_script}" ]; then
        source "${highlighting_script}"
        break
    fi
done
unset highlighting_script
