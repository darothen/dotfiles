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
case "$HOST" in
    legion | l00*)
        mach_color=$fg_bold[red];;
    yslogin*)
        mach_color=$fg_bold[yellow];;
    svante*)
        mach_color=$fg_bold[purple];;
    newton*)
        mach_color=$fg_bold[cyan];;
    *ubuntu*)
        mach_color=$fg_bold[white];;
    *)
        mach_color=$fg_bold[blue];;
esac

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
