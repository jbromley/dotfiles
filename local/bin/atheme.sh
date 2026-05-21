#! /usr/bin/env bash
# Preview and select Alacritty themes

THEMES_DIR="$HOME/.config/alacritty/themes"
CONFIG="$HOME/.config/alacritty/alacritty.toml"

# Internal handler: update config to the given theme (called on focus change)
if [[ "$1" == "--_apply" ]]; then
    sed -i "s|^import = \[.*\]|import = [\"~/.config/alacritty/themes/${2}.toml\"]|" "$CONFIG"
    exit 0
fi

show_preview=false
for arg in "$@"; do
    case "$arg" in
        --preview|-p) show_preview=true ;;
    esac
done

if [[ ! -d "$THEMES_DIR" ]]; then
    echo "Themes directory not found: $THEMES_DIR" >&2
    exit 1
fi

if [[ ! -f "$CONFIG" ]]; then
    echo "Alacritty config not found: $CONFIG" >&2
    exit 1
fi

current=$(grep '^import' "$CONFIG" | sed 's|.*themes/||; s|\.toml.*||' | head -1)
[[ -z "$current" ]] && current="(none)"

# Backup config and restore it on exit; cleared on successful selection
backup=$(mktemp --suffix=.toml)
cp "$CONFIG" "$backup"
trap 'cp "$backup" "$CONFIG"; rm -f "$backup"' EXIT

export ATHEME_SCRIPT
ATHEME_SCRIPT="$(readlink -f "$0")"

# reverse-list keeps prompt at bottom but displays items top-to-bottom in stdin
# order, so pos(N) maps directly to the line number in the sorted list.
themes_list=$(find "$THEMES_DIR" -maxdepth 1 -name '*.toml' | xargs -n1 basename | sed 's/\.toml$//' | sort)

start_pos=$(echo "$themes_list" | grep -n "^${current}$" | cut -d: -f1)
[[ -z "$start_pos" ]] && start_pos=1

fzf_args=(
    --prompt="Theme > "
    --header="Current: $current  |  Enter: apply  Esc: cancel"
    --height=50%
    --layout=reverse-list
    --bind='focus:execute-silent(bash "$ATHEME_SCRIPT" --_apply {})'
    --bind="load:pos($start_pos)"
)

if $show_preview; then
    fzf_args+=(
        --preview='colortest-16'
        --preview-window="right:60%"
    )
fi

selected=$(echo "$themes_list" | fzf "${fzf_args[@]}")

if [[ -n "$selected" ]]; then
    trap - EXIT
    rm -f "$backup"
    sed -i "s|^import = \[.*\]|import = [\"~/.config/alacritty/themes/${selected}.toml\"]|" "$CONFIG"
    echo "Theme set to: $selected"
fi
