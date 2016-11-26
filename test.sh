#!/bin/bash
set -x

./build.sh \
&& sudo cp /boot/raspbian/kernel7.img /boot/kernel7.img.raspbian \
&& sudo cp kernel7.img /boot \
&& sudo reboot
