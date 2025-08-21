set -l dircolors (test -f ~/.dircolors; and dircolors -b ~/.dircolors; or dircolors -b)
set -gx LS_COLORS (string match -r --groups-only "LS_COLORS='([^']*)'" -- $dircolors)
