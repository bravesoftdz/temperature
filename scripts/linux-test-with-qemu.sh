#!/bin/bash
set -x

mkdir -p build-test-output

./scripts/linux-test-with-qemu-monitor-commands.sh | \
 qemu-system-arm \
 -M versatilepb \
 -cpu cortex-a8 \
 -kernel kernel.bin \
 -m 256M \
 -usb \
 -display none \
 -serial mon:stdio |& tee build-test-output/raw.log | egrep -iv '^(alsa|pulseaudio:|audio:)' > build-test-output/serial.log

convert build-test-output/screen-01.ppm build-test-output/screen-01.png
convert build-test-output/screen-02.ppm build-test-output/screen-02.png
rm build-test-output/*.ppm

cat build-test-output/serial.log
file build-test-output/*
