#!/bin/bash
echo ".................................................................."
echo "Installing stumpwm"
export HOME=/home/amsha
cd /home/amsha/packages_external/stumpwm
make clean
echo amsha
echo $HOME
make
sudo make install
echo ".................................................................."
