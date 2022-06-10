# Overload system default 256-color palette with precise gruvbox colors.
source "$HOME/.local/share/nvim/site/pack/packer/start/gruvbox/gruvbox_256palette.sh"
# Enable bash completions.
source "/usr/share/bash-completion/bash_completion"

# Required to sign git committs.
export GPG_TTY=$(tty)
# Choose our default text editor.
export EDITOR=nvim

# Enable vi mode.
set -o vi
