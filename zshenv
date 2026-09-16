# Zsh environment customizations

# Make sure language is set properly.
export LANG=en_US.UTF-8
export LC_NUMERIC=en_US.UTF-8

# Editor
export EDITOR=hx
export GIT_EDITOR=hx

# Zsh environment variables
export HISTFILE="${HOME}/.zhistory"
export HISTSIZE=65536
export SAVEHIST=65536
export HISTORY_IGNORE="(ls|cd|pwd|exit|cd)*"

# Fzf customizations
export FZF_DEFAULT_COMMAND='ag --hidden -g ""'
export FZF_CTRL_T_OPTS="--prompt 'file> '"
export FZF_ALT_C_OPTS="--prompt 'cd> '"
export FZF_CTRL_R_OPTS="--prompt 'history> '"

# Don't let Python venv add anything to the prompt.
export VIRTUAL_ENV_DISABLE_PROMPT=yes

# Use color in GCC warnings and errors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'
export COLORTERM=truecolor

# Set up environments for various tools.

# Local executables directory
[ -d "$HOME/.local/bin" ] &&  path+=${HOME}/.local/bin

# Rust
[ -f "${HOME}/.cargo/env" ] && source "${HOME}/.cargo/env"

# Haskell
[ -f "${HOME}/.ghcup/env" ] && source "${HOME}/.ghcup/env"

# ROS 2
export ROS_DOMAIN_ID=17
# export RMW_IMPLEMENTATION=rmw_cyclonedds_cpp

# Configure the path.
typeset -Ux PATH path
path=(${HOME}/.local/bin $path)

# If these paths exist, add them to PATH.
extra_paths=(/usr/lib/llvm-15/bin)
[ -n "${GOBIN}" ] && extra_paths+=("${GOBIN}")

for extra_path in $extra_paths; do
    [ -d "$extra_path" ] && path+=("$extra_path")
done
