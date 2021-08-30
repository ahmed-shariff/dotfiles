export TERM="xterm-256color"
# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="agnoster"

# Set list of themes to load
# Setting this variable when ZSH_THEME=random
# cause zsh load theme from this variable instead of
# looking in ~/.oh-my-zsh/themes/
# An empty array have no effect
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

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
ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

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

POWERLINE_NO_BLANK_LINE="none"
#POWERLINE_PATH="short"

COLOR_1_BG=60
COLOR_1_FG=255
COLOR_2_BG=24
COLOR_2_FG=255
COLOR_3_BG=202
COLOR_3_FG=black
# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
    git
    poetry
)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/rsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"



# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory autocd extendedglob nomatch notify
unsetopt beep
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
#zstyle :compinstall filename '/home/amsha/.zshrc'

fpath+=~/.zfunc

autoload -Uz compinit
compinit
# End of lines added by compinstall

# Load platform spcific setup
if [ -f "$HOME/.rc_platform_setup" ]; then
    source "$HOME/.rc_platform_setup"
fi

alias ls='ls --color=auto'

function proxy_on() {
    export no_proxy="localhost,127.0.0.1,localaddress,.localdomain.com"

    if (( $# > 0 )); then
        valid=$(echo $@ | sed -n 's/\([0-9]\{1,3\}.\)\{4\}:\([0-9]\+\)/&/p')
        if [[ $valid != $@ ]]; then
            >&2 echo "Invalid address"
            return 1
        fi

        export http_proxy="http://$1/"
        export https_proxy=$http_proxy
        export ftp_proxy=$http_proxy
        export rsync_proxy=$http_proxy
        echo "Proxy environment variable set."
        return 0
    fi

    #echo -n "username: "; read username
    #if [[ $username != "" ]]; then
    #    echo -n "password: "
    #    read -es password
    #    local pre="$username:$password@"
    #fi

    echo -n "server: "; read server
    echo -n "port: "; read port
    export http_proxy="http://$pre$server:$port/"
    export https_proxy=$http_proxy
    export ftp_proxy=$http_proxy
    export rsync_proxy=$http_proxy
    export HTTP_PROXY=$http_proxy
    export HTTPS_PROXY=$http_proxy
    export FTP_PROXY=$http_proxy
    export RSYNC_PROXY=$http_proxy
}


function proxy_onf() {
    export no_proxy="localhost,127.0.0.1,localaddress,.localdomain.com"
    export http_proxy="http://$1/"
    export https_proxy=$http_proxy
    export ftp_proxy=$http_proxy
    export rsync_proxy=$http_proxy
    export HTTP_PROXY=$http_proxy
    export HTTPS_PROXY=$http_proxy
    export FTP_PROXY=$http_proxy
    export RSYNC_PROXY=$http_proxy
}


function proxy_off(){
    unset http_proxy
    unset https_proxy
    unset ftp_proxy
    unset rsync_proxy
    echo -e "Proxy environment variable removed."
}

function activate_bb(){
    rc6_enable=$(cat /sys/class/drm/card0/power/rc6_enable)
    if [ $rc6_enable -ne 0 ]; then
	echo -n "i915 rc6 is not 0, do you want to continue? [Y/n]  "
	read -n 1 input
	echo -e "\n"
	if [ $input = Y ]; then
	    echo "k, whatever!!"
	    sudo systemctl start bumblebeed
	    systemctl status bumblebeed
	fi
    else
	sudo systemctl start bumblebeed
	systemctl status bumblebeed
    fi
}

function get_music(){
    if [[ "$#" -eq 1 ]]; then
        youtube-dl -x --no-mtime --audio-quality 0 --audio-format mp3 $(python -c "print(\"$1\".split(\"&list\")[0])")
    elif [[ "$#" -eq 2 ]]; then
         youtube-dl -x --no-mtime --audio-quality 0 --audio-format mp3 $(python -c "print(\"$1\".split(\"&list\")[0])") --exec "mv {} \"temp\"; ffmpeg -i \"temp\" -ss $2 {};rm \"temp\""
    elif [[ "$#" -eq 3 ]]; then
        youtube-dl -x --no-mtime --audio-quality 0 --audio-format mp3 $(python -c "print(\"$1\".split(\"&list\")[0])") --exec "mv {} \"temp\"; ffmpeg -i \"temp\" -ss $2 -t $3 {};rm \"temp\""
    else
        echo "huh?"
    fi
}

function trim_youtube_dl()
{
    echo "$2  $3"
    # mv "$1" "temp"
    # ffmpeg -i "temp" -ss "$2" -t "$3" "$1"
}

function lock_cmd(){
    alias cd="printf 'Nice try darling!'"
    alias opera="printf 'Nice try darling!'"
    alias sudo="printf 'Nice try darling!'"
    alias ls="printf 'Nice try darling!'"
    alias tilix="printf 'Nice try darling!'"
    alias xfce4-terminal="printf 'Nice try darling!' "
    alias unset="printf 'Nice try darling!'"
    alias source="printf 'Nice try darling!'"
    alias emacs="printf 'Nice try darling!'"
    alias emacsclient="printf 'Nice try darling!'"
    alias et="printf 'Nice try darling!'"
    alias vi="printf 'Nice try darling!'"
    alias nano="printf 'Nice try darling!'"
    alias sed="printf 'Nice try darling!'"
    alias bash="printf 'Nice try darling!'"
    alias zsh="printf 'Nice try darling!'"
    alias sh="printf 'Nice try darling!'"
    alias rm="printf 'Nice try darling!'"
    
    alias alias="printf 'Nice try darling!'"
    # echo "hahahahahah"
}

export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/opt/cuda/lib64:/opt/intel/lib/intel64:"
export CUDA_HOME=/opt/cuda
export LIB=lib
export PATH=$PATH:$CUDA_HOME/lib64:/home/amsha/.gem/ruby/2.7.0/bin
export GEM_HOME="$HOME/.gems"

alias uniproxy="proxy_onf cachex.pdn.ac.lk:3128"
alias unisetup="uniproxy;(opera & exit); exit"
alias et="emacs -nw"
#alias ls="tyls"
alias uniwork="source virtualenv/pytorch/bin/activate;cd /media/Files/Research/FoodClassification"
alias activate_nm="sudo systemctl start NetworkManager;systemctl status NetworkManager"
alias config-git='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
alias print_diamond="lpr -U $LRP_USERNAME -P $LRP_PRINTER"
alias scrot-clip="scrot -d 3 ~/temp.png && xclip -selection clipboard -t image/png -i ~/temp.png && rm ~/temp.png"
#alias activate_bb="sudo systemctl start bumblebeed;systemctl status bumblebeed"


unset SBCL_HOME

# torch-----------------------------------------------------------------------
#. /home/amsha/torch/install/bin/torch-activate

# Tilix-----------------------------------------------------------------------
if [ $TILIX_ID ] || [ $VTE_VERSION ]; then
        source /etc/profile.d/vte.sh
fi

# powerline-------------------------------------------------------------------
#. /usr/lib/python3.6/site-packages/powerline/bindings/bash/powerline.sh


# gcloud----------------------------------------------------------------------
# The next line updates PATH for the Google Cloud SDK.
# if [ -f '/home/amsha/packages_external/google-cloud-sdk/path.bash.inc' ]; then source '/home/amsha/packages_external/google-cloud-sdk/path.bash.inc'; fi

# The next line enables shell command completion for gcloud.
# if [ -f '/home/amsha/packages_external/google-cloud-sdk/completion.bash.inc' ]; then source '/home/amsha/packages_external/google-cloud-sdk/completion.bash.inc'; fi

# added by Anaconda3 installer
# export PATH="/home/amsha/anaconda3/bin:$PATH"

# neofetch--------------------------------------------------------------------
neofetch

# rclone----------------------------------------------------------------------
source .sh_functions/rclone.sh 

export PATH="$HOME/.poetry/bin:$PATH"
