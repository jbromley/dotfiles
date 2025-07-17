# Zsh environment customizations

# Make sure language is set properly.
export LANG=en_US.UTF-8
export LC_NUMERIC=en_US.UTF-8

# Editor
export EDITOR=emacsclient
export VISUAL=emacs
export GIT_EDITOR='emacsclient --tty'
export ALTERNATE_EDITOR='emacs -Q -nw'

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

# Golang
if [ -d "${HOME}/go" ]; then
    export GOPATH="${HOME}/go"
    export GOBIN="${GOPATH}/bin"
fi

# Mise version manager
mise_executable=${HOME}/.local/bin/mise
if [ -x ${mise_executable} ]; then
  eval "$(${mise_executable} activate zsh)"
fi

# OCaml environment
[[ ! -r '${HOME}/.opam/opam-init/init.zsh' ]] || source '${HOME}/.opam/opam-init/init.zsh' &> /dev/null

opam_executable="${HOME}/.local/share/mise/installs/opam/latest/bin/opam"
if [ -x "$opam_executable" ]; then
  eval "$($opam_executable env)"
fi

# Rust
[ -f "${HOME}/.cargo/env" ] && source "${HOME}/.cargo/env"

# Haskell
[ -f "${HOME}/.ghcup/env" ] && source "${HOME}/.ghcup/env"

# ROS 2
export ROS_DOMAIN_ID=17
export RMW_IMPLEMENTATION=rmw_cyclonedds_cpp

# Configure the path.
typeset -Ux PATH path
path=(${HOME}/.local/bin $path)

# If these paths exist, add them to PATH.
extra_paths=(/usr/lib/llvm-15/bin)
[ -n "${GOBIN}" ] && extra_paths+=("${GOBIN}")

for extra_path in $extra_paths; do
    [ -d "$extra_path" ] && path+=("$extra_path")
done
