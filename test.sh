#!/bin/bash
set -x

./build.sh \
&& sudo cp /boot/raspbian/kernel7.img /boot/kernel7.img.missioncontrol \
&& sudo cp kernel7.img /boot \
&& sudo cp /boot/raspbian/cmdline.txt /boot/cmdline.txt.missioncontrol \
&& sudo cp cmdline.txt /boot \
&& sudo reboot
