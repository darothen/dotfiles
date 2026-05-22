alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'

alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'
# -> Prevents accidentally clotbbering files.

alias mkdir='mkdir -p'
alias tree='tree -Csuh'    #  Nice alternative to 'recursive ls' ...


if ls --color > /dev/null 2>&1; then # GNU `ls`
    colorflag="--color=auto"
else # OS X `ls`
    colorflag="-G"
fi
alias l="ls -lF ${colorflag}"
alias ll="ls -lthF ${colorflag}"
alias la="ls -laF ${colorflag}"
alias lr="ls -ltrhF ${colorflag}"
alias lsd="ls -lF ${colorflag} | grep --color=never '^d'"
alias ls="command ls ${colorflag}"
#export LS_COLORS='no=00:fi=00:di=01;34:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.gz=01;31:*.bz2=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.avi=01;35:*.fli=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.ogg=01;35:*.mp3=01;35:*.wav=01;35:'

# Pretty-print of some PATH variables:
alias path='echo -e ${PATH//:/\\n}'
alias libpath='echo -e ${LD_LIBRARY_PATH//:/\\n}'

alias du='du -kh'    # Makes a more readable output.
alias df='df -kTh'

alias c="clear"
alias path='echo -e ${PATH//:/\\n}'
alias now='date +"%T"'
alias nowtime=now
alias nowdate='date +"%d-%m-%Y"'
if command -v rg &> /dev/null; then
    alias grep='rg'
else
    alias grep='grep --color=auto'
fi
alias tf="tail -n 500 -f"

# Editors and utilities
alias em="emacs -nw"

# Fast connect to known machines
alias svante="ssh -Y darothen@svante-login.mit.edu"
alias yellowstone="ssh -Y darothen@yellowstone.ucar.edu"
alias legion="ssh -Y darothen@legion.mit.edu"

# Google Cloud SDK quick aliases

# Authentication
alias glogin="gcloud auth login"
alias gapplogin="gcloud auth application-default login"
alias glogout="gcloud auth revoke"
# Configuration
alias gconfig="gcloud config list"
alias gconfigs="gcloud config configurations list"
alias gsetconfig="gcloud config configurations activate"

# Compute instances
alias gclist="gcloud compute instances list"
alias gcstart="gcloud compute instances start"
alias gcstop="gcloud compute instances stop"
alias gcreset="gcloud compute instances reset"
alias gcdescribe="gcloud compute instances describe"
alias gcssh="gcloud compute ssh"
alias gcscp="gcloud compute scp"

# Storage operations
alias gsls="gcloud storage ls"
alias gscp="gcloud storage cp"
alias gsmv="gcloud storage mv"
alias gsrm="gcloud storage rm"
alias gsmb="gcloud storage buckets create"
alias gsrb="gcloud storage buckets delete"