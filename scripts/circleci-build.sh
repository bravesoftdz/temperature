#!/bin/bash
set -x
cd src
rm -rf artifacts/ *.bin *.elf *.img *.o *.ppu
mkdir -p artifacts/kernels artifacts/build-test-output
fpc -dTARGET_QEMUARM7A -B -Tultibo -O2 -Parm -CpARMV7A -WpQEMUVPB @/root/ultibo/core/fpc/bin/qemuvpb.cfg \
 ptemperature.lpr && \
cp -a kernel.bin artifacts/kernels && \
fpc -dTARGET_RPI3 -B -Tultibo -O2 -Parm -CpARMV7A -WpRPI3B @/root/ultibo/core/fpc/bin/rpi3.cfg \
 ptemperature.lpr && \
cp -a kernel7.img artifacts/kernels && \
cp -a run-qemu artifacts/kernels
