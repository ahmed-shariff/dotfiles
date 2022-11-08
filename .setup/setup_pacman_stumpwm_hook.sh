#!/bin/bash

# This script creates:
# - The post-install pacman hook that will run every time sbcl is updated

# in Arch use stumpwm-git instead!!


hook_pacman="/etc/pacman.d/hooks/stumpwm-post.hook"
hook_script="$HOME/.stumpwm.d/pacman_hook.sh"
user=$USER

die() {
  echo "$@"
  exit 1
}

command -v pacman &>/dev/null || die "pacman not installed. Exiting ..."

sudo mkdir -p "${hook_pacman%/*}"

echo "Generating pacman hook ..."
echo "User is $user"

sudo tee <<EOF "${hook_pacman}" &>/dev/null
[Trigger]
Operation = Install
Operation = Upgrade
Type = Package
Target = sbcl
[Action]
Description = Rebuilding stumpwm with new sbcl
When = PostTransaction
Exec = ${hook_script}
EOF

# echo "Generating hook script ..."

# tee << EOF "${hook_script}" &>/dev/null
# #!/bin/bash
# echo ".................................................................."
# echo "Installing stumpwm"
# export HOME=$HOME
# cd $HOME/packages_external/stumpwm
# make clean
# echo $user
# echo \$HOME
# make
# sudo make install
# echo ".................................................................."
# EOF

# chmod +x "${hook_script}"
