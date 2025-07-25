# ======================================================================
# Zsh configuration file
# Inspired by the blog post "Understanding and Configuring Zsh"
# (https://thevaluable.dev/zsh-install-configure/) and the from-scratch
# configuration on GitHub at https://github.com/Phantas0s/.dotfiles/tree/master/zsh
#======================================================================

# Enable Powerlevel10k instant prompt.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Emacs key bindings
bindkey -e

# Changing directories
setopt AUTO_CD
setopt PUSHD_IGNORE_DUPS
setopt CORRECT
setopt EXTENDED_GLOB

# History
setopt EXTENDED_HISTORY
setopt INC_APPEND_HISTORY
setopt SHARE_HISTORY
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_SAVE_NO_DUPS
setopt HIST_VERIFY
setopt APPEND_HISTORY
setopt HIST_NO_STORE
setopt HIST_REDUCE_BLANKS

# Aliases
[ -f ${HOME}/.aliases ] && source ${HOME}/.aliases

# Plugins
zmodload zsh/zutil
plugin_dir=${HOME}/.zsh
fpath=(${plugin_dir} $fpath)

# bd
# source ${plugin_dir}/zsh-bd/bd.zsh

# Fish-like auto-suggestions
zcompile ~/.zsh/zsh-autosuggestions/{zsh-autosuggestions.zsh,src/**/*.zsh}
source ${plugin_dir}/zsh-autosuggestions/zsh-autosuggestions.zsh
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=#606060"

# Syntax highlighting
zcompile ~/.zsh/zsh-syntax-highlighting/{zsh-syntax-highlighting.zsh,highlighters/main/*.zsh}
source ${plugin_dir}/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

#
# Completion
#

# ROS 2 colcon
colcon_comp=/usr/share/colcon_argcomplete/hook/colcon-argcomplete.zsh
[ -f ${colcon_comp} ] && source ${colcon_comp}

ros2_arg_comp=${ROS_DIR}/share/ros2cli/environment/ros2-argcomplete.zsh
[ -f ${ros2_arg_comp} ] && source ${ros2_arg_comp}

# zsh completion sources
fpath=(${plugin_dir}/zsh-completions/src $fpath)
autoload -Uz compinit; compinit
_comp_options+=(globdots)
source ${plugin_dir}/completion.zsh

# bash completions
autoload bashcompinit; bashcompinit

#
# Interactive tools
#

# Fzf
source <(fzf --zsh)
bindkey -s '^V' 'nvim $(fzf --preview "bat --color always {}");^M'
# bindkey -s '^W' 'fzf --preview="bat --color always {}" --bind shift-up:preview-page-up,shift-down:preview-page-down;^M'

# zoxide
[ -x /usr/bin/zoxide ] || [ -x /usr/local/bin/zoxide ] && eval "$(zoxide init zsh)" 

# Dircolors
[ -f ${HOME}/.dircolors ] && eval "$(dircolors -b ${HOME}/.dircolors)"

# Prompt
function set_term_title() {
    title=$(pwd | sed -e "s|${HOME}|~|")
    echo -ne "\e]0;${title}\a"
}
precmd_functions+=(set_term_title)

# Set up atuin
eval "$(atuin init zsh)"

#
# Functions
#

# Check ps for a process
function psinfo() {
    if [ -z "$1" ]; then
        echo "Usage: psinfo <regex>"
    else
        ps -ef | grep -E "$1" | grep -v grep
    fi
}

# Use z to jump and push the directory.
function zp() {
    if [ -z "$1" ]; then
        echo "Usage: zp DIRECTORY"
    else
        pushd $(zoxide query "$1")
    fi
}

# Push or pop just using `p` and context.
function p() {
    case "$#" in
    0)
        popd
        ;;
    1)
        pushd "$1"
        ;;
    *)
        echo "p [DIR]"
        false
        ;;
    esac
}

# Open a file in nvim using an argument or fzf
function n() { 
  nvim "${1:-"$(fzf)"}" 
} 

# Open a file in emacs using an argument or fzf
function e() { 
  emacs "${1:-"$(fzf)"}" &
} 

# Run a program under NVidia when in hybrid graphics mode
function nvrun() {
  __NV_PRIME_RENDER_OFFLOAD=1 __GLX_VENDOR_LIBRARY_NAME=nvidia "$@"
}

# Change to directory when exiting yazi
function yy() {
	local tmp="$(mktemp -t "yazi-cwd.XXXXXX")"
	yazi "$@" --cwd-file="$tmp"
	if cwd="$(cat -- "$tmp")" && [ -n "$cwd" ] && [ "$cwd" != "$PWD" ]; then
		cd -- "$cwd"
	fi
	rm -f -- "$tmp"
}

#
# Prompt
#

eval "$(starship init zsh)"

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
# source ~/.zsh/powerlevel10k/powerlevel10k.zsh-theme
# [[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
