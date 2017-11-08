#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '


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

export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/opt/cuda/lib64:"
export CUDA_HOME=/opt/cuda
export LIB=lib
export PATH=$PATH:$CUDA_HOME/lib64

alias uniproxy="proxy_onf cachex.pdn.ac.lk:3128"
alias unisetup="uniproxy;(opera & exit); exit"
alias et="emacs -nw"
#alias ls="tyls"
alias uniwork="source virtualenv/tf12-built/bin/activate;cd /media/Files/Research/FoodClassification/models"
alias activate_nm="sudo systemctl start NetworkManager;systemctl status NetworkManager"
#alias activate_bb="sudo systemctl start bumblebeed;systemctl status bumblebeed"


unset SBCL_HOME

#torch-----------------------------------------------------------------------
#. /home/amsha/torch/install/bin/torch-activate

#Tilix-----------------------------------------------------------------------
if [ $TILIX_ID ] || [ $VTE_VERSION ]; then
        source /etc/profile.d/vte.sh
fi
#powerline-------------------------------------------------------------------
. /usr/lib/python3.6/site-packages/powerline/bindings/bash/powerline.sh


#gcloud----------------------------------------------------------------------
# The next line updates PATH for the Google Cloud SDK.
if [ -f '/home/amsha/packages_external/google-cloud-sdk/path.bash.inc' ]; then source '/home/amsha/packages_external/google-cloud-sdk/path.bash.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/home/amsha/packages_external/google-cloud-sdk/completion.bash.inc' ]; then source '/home/amsha/packages_external/google-cloud-sdk/completion.bash.inc'; fi
