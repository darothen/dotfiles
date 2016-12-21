# ============================================================== #
#
# PERSONAL BASH/CONFIG ($HOME/.bashrc)
#
# Last modified: Friday, June 26, 2014
# Author: Daniel Rothenberg <darothen@mit.edu>
#
# This configuration file is the stub I use to modify my working environment
# on *nix systems. Some of the particular PATH modfications may need to be
# altered depending on where the script is ported, but anything after the
# user-configuration section should be fine.
#
# with inspiration from:
#     - http://www.tldp.org/LDP/abs/html/sample-bashrc.html
#
# ============================================================== #


# Terminal config/info/MOTD

case $TERM in
    xterm*)
        export TITLEBAR='\[\033]0;\u@\h:\w\007'
        ;;
    *)
        export TITLEBAR=""
        ;;
esac

date

# -------------------------------------------------------------- #

# Begin setting up PATH
#export PATH=/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/texbin:$PATH

# Detect platform
platform='unknown'
unamestr=$(uname)
hostnamestr=$(hostname)
if [[ "$unamestr" == 'Linux' ]]; then
   platform='linux'
elif [[ "$unamestr" == 'FreeBSD' ]]; then
   platform='freebsd'
elif [[ "$unamestr" == 'Darwin' ]]; then
   platform="mac"
fi
export PLATFORM=$platform
export HOST=$hostnamestr

echo "Configuring for platform $PLATFORM on host $HOST..."

# Load shell dotfiles
for file in ~/.bash_{machine,aliases,exports,functions,prompt}; do
    [ -r "$file" ] && [ -f "$file" ] && source "$file"
done
unset file

## Set some shell options
# case-insensitive globbing
shopt -s nocaseglob

# append bash history rather than overwrite
shopt -s histappend

# autocorrect typos in path names via 'cd'
shopt -s cdspell

# Be sure that home/bin is at front of path
export PATH=$HOME/bin:$PATH
