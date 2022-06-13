# Use bash-completion, if available
[[ $PS1 && -f /usr/share/bash-completion/bash_completion ]] && \
    . /usr/share/bash-completion/bash_completion

# Required to sign git commits.
export GPG_TTY=$(tty)
# Choose our default text editor.
export EDITOR=nvim

# Enable vi mode.
set -o vi

# Prompt customization.
PS1="\[\e[0;35m\][ \[\e[0;3m\]\u\[\e[0;3m\]@\[\e[0;3m\]\H\[\e[0;35m\] ] [ \[\e[0m\]\w\[\e[0;35m\] ] \[\e[0m\]"
