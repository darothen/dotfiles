# Load .bash_profile which does all the setup
if [[ "$-" != *i* ]]; then
    return
fi
source ~/.bash_profile;

. "$HOME/.local/bin/env"
. "$HOME/.cargo/env"

