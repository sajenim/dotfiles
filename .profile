# Invoke .bashrc on startup.
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
        . "$HOME/.bashrc"
    fi
fi

# Declare our path.
PATH=$PATH:/home/minnie/.local/bin
