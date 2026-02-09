#!/usr/bin/env zsh
# Base ZSH configuration - linked to ~/.zshrc for all machines
# Sources machine-specific configuration from dotfiles directory

# ============================================================================
# Helper Functions
# ============================================================================

# Safe file sourcing with error handling
safe_source() {
    local file="$1"
    local description="${2:-configuration file}"

    if [[ -f "$file" ]]; then
        if [[ -r "$file" ]]; then
            source "$file"
        else
            echo "Warning: $description exists but is not readable: $file" >&2
        fi
    else
        echo "Warning: $description not found: $file" >&2
    fi
}

# Safe PATH addition with directory validation
safe_add_path() {
    local dir="$1"
    local position="${2:-append}"  # append or prepend
    local silent="${3:-false}"      # suppress warnings

    if [[ -d "$dir" ]]; then
        if [[ "$position" == "prepend" ]]; then
            export PATH="$dir:$PATH"
        else
            export PATH="$PATH:$dir"
        fi
    elif [[ "$silent" != "true" ]]; then
        echo "Warning: Directory not found, skipping PATH addition: $dir" >&2
    fi
}

# ============================================================================
# Oh-My-Zsh Framework Setup
# ============================================================================

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="darothen"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# ============================================================================
# User Configuration - Platform Detection
# ============================================================================

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

echo "Configuring zsh for platform $PLATFORM on host $HOST...\n"

# Platform-specific plugins
if [[ "$PLATFORM" == "mac" ]]; then
    plugins=(git github macos python)
else
    plugins=(git github python)
fi

# Source bash configuration files with safety checks
for file in ~/.bash_{aliases,exports,functions}; do
    safe_source "$file" "bash config: $(basename $file)"
done
unset file

# Ensure home/bin is at front of path
safe_add_path "$HOME/bin" "prepend" true

# Load zsh custom plugins
source $ZSH/oh-my-zsh.sh

# ============================================================================
# Environment Configuration
# ============================================================================

# You may need to manually set your language environment
export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Source machine-specific configuration after oh-my-zsh to ensure aliases aren't overwritten
safe_source "$HOME/.bash_machine" "machine-specific bash config"

# Fix for ghostty / tmux
if [[ "$TERM_PROGRAM" == "ghostty" ]]; then
    export TERM=xterm-256color
fi

# ============================================================================
# Machine-Specific Configuration
# ============================================================================
# Detect current machine and source machine-specific config from dotfiles

# Determine dotfiles directory (where this script is symlinked from)
# Resolve the symlink to find the actual dotfiles location
if [[ -L "$HOME/.zshrc" ]]; then
    DOTFILES_ZSH_DIR="$(dirname "$(readlink "$HOME/.zshrc")")"
else
    # Fallback to hardcoded path if .zshrc is not a symlink
    DOTFILES_ZSH_DIR="$HOME/software/dotfiles/zsh"
fi

# Try to detect machine from hostname
DETECTED_MACHINE=""

# On macOS, also try ComputerName which is more reliable
HOSTNAME_TO_CHECK="$HOST"
if [[ "$PLATFORM" == "mac" ]] && command -v scutil &> /dev/null; then
    COMPUTER_NAME=$(scutil --get ComputerName 2>/dev/null)
    if [[ -n "$COMPUTER_NAME" ]]; then
        HOSTNAME_TO_CHECK="$COMPUTER_NAME"
    fi
fi

case "$HOSTNAME_TO_CHECK" in
    *brightband-gcp*)
        DETECTED_MACHINE="brightband-gcp";;
    *brightband*)
        DETECTED_MACHINE="brightband";;
    *mbp* | *MacBook*)
        DETECTED_MACHINE="mbp";;
    *roth-home*)
        DETECTED_MACHINE="roth-home";;
    *climacell*)
        DETECTED_MACHINE="climacell";;
esac

# Fallback: detect machine from .bash_machine symlink if hostname detection failed
if [[ -z "$DETECTED_MACHINE" ]] && [[ -L "$HOME/.bash_machine" ]]; then
    BASH_MACHINE_TARGET=$(readlink "$HOME/.bash_machine")
    # Extract machine name from .bash_machine.MACHINE symlink
    DETECTED_MACHINE="${BASH_MACHINE_TARGET##*.bash_machine.}"
fi

# Source machine-specific configuration if it exists
if [[ -n "$DETECTED_MACHINE" ]]; then
    MACHINE_ZSHRC="$DOTFILES_ZSH_DIR/.zshrc.$DETECTED_MACHINE"
    if [[ -f "$MACHINE_ZSHRC" ]]; then
        source "$MACHINE_ZSHRC"
    else
        echo "Warning: Machine-specific config not found: $MACHINE_ZSHRC" >&2
    fi
else
    echo "Warning: Could not detect machine from hostname: $HOST" >&2
    echo "Skipping machine-specific configuration." >&2
fi

# ============================================================================
# End of Base Configuration
# ============================================================================
