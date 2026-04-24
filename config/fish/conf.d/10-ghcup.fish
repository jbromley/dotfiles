# Set up for Haskell
if [-d $HOME/.ghcup]
    set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME
    fish_add_path $HOME/.ghcup/bin
end
if [ -d $HOME/.cabal ]
    fish_add_path $HOME/.cabal/bin
end
