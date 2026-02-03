# Custom ZSH theme, based on http://stevelosh.com/blog/2010/02/my-extravagant-zsh-prompt/

COL_BRACKET="%{$reset_color%}%{$fg[blue]%}"
COL_DEFAULT="%{$reset_color%}%{$fg[red]%}"

function collapse_pwd {
    echo $(pwd | sed -e "s,^$HOME,~,")
}

function get_RAM {
  free -m | awk '{if (NR==3) print $4}' | xargs -i echo 'scale=1;{}/1000' | bc
}

function get_nr_jobs() {
  jobs | wc -l
}

function prompt_char {
    # git branch >/dev/null 2>/dev/null && echo '±' && return
    echo '>'
}

function virtualenv_info {
    [ $VIRTUAL_ENV ] && echo '('`basename $VIRTUAL_ENV`') '
}


# Slow git fix from https://gist.github.com/msabramo/2355834
# function git_prompt_info() {
#   ref=$(git symbolic-ref HEAD 2> /dev/null) || return
#   echo "$ZSH_THEME_GIT_PROMPT_PREFIX${ref#refs/heads/}$ZSH_THEME_GIT_PROMPT_SUFFIX"
# }

# Determine which machine we're on for coloring
path_color=$fg_bold[cyan];
# Valid colors - red, yellow, purple, cyan, white, magenta, blue, ...
case "$HOST" in
    *ubuntu*)
        mach_color=$fg_bold[red];;
    roth-home*)
        mach_color=$fg_bold[cyan];;
    bb-* | brightband-*)
        mach_color=$fg_bold[white]$bg[red];;
    *)
        mach_color=$fg_bold[blue];;
esac
# More generally, detect if we're on GCP
# Check for GCP without requiring sudo by reading DMI data directly
if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    # Check /sys/class/dmi/id/product_name which doesn't require root
    if [ -f /sys/class/dmi/id/product_name ]; then
        if grep -q "Google Compute Engine" /sys/class/dmi/id/product_name 2>/dev/null; then
            mach_color=$fg_bold[white]$bg[red];
        fi
    fi
fi

# Set path color to magenta for GCP machines
IS_GCP_MACHINE=false

# Also check if DMI detected GCP (indicated by white-on-red mach_color)
if [[ "$OSTYPE" == "linux-gnu"* ]] && [ -f /sys/class/dmi/id/product_name ]; then
    if grep -q "Google Compute Engine" /sys/class/dmi/id/product_name 2>/dev/null; then
        IS_GCP_MACHINE=true
    fi
fi

# Apply magenta path color for GCP machines only
if [[ "$IS_GCP_MACHINE" == "true" ]]; then
    path_color=$fg_bold[magenta];
fi

PROMPT='%{$fg[blue]%}[%{$fg[red]%}%t%{$fg[blue]%}] '
PROMPT+='%{$fg_bold[white]%}%n '
PROMPT+='%{$reset_color%}%{$fg[red]%}at '
PROMPT+='%{$mach_color%}%M%{$reset_color%}%{$fg[red]%} '
PROMPT+='in %{$path_color%}%~%{$reset_color%} '
PROMPT+='$(git_prompt_info) '
PROMPT+='
%{$reset_color%}%{$fg[red]%}%h '
PROMPT+='$(virtualenv_info)%{$reset_color%}%{$fg[blue]%}$(prompt_char)%{$reset_color%} '

# RPROMPT=""

# ZSH_THEME_GIT_PROMPT_CACHE=""
ZSH_THEME_GIT_PROMPT_PREFIX="%{$reset_color%}%{$fg[red]%}on %{$fg_bold[yellow]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX=" %{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY="${COL_BRACKET}(%{$fg[red]%}✗${COL_BRACKET})"
ZSH_THEME_GIT_PROMPT_CLEAN="${COL_BRACKET}(%{$fg[green]%}✓${COL_BRACKET})"
