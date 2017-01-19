#!/bin/bash

PROGRAM=pqemuframebuffer.lpr

OUTPUT=out
QEMU_SCRIPT=run-qemu.tmp

which fpc
if [ $? == 0 ]
then
    FPC_COMPILER=fpc
else
    DOCKER_FPC_IMAGE=markfirmware/ufpc
    FPC_COMPILER="docker run --rm -v $(pwd):/workdir $DOCKER_FPC_IMAGE"
    echo fpc not found ... using docker image $DOCKER_FPC_IMAGE
fi

function build {
    rm -rf obj && \
    mkdir -p obj && \
    $FPC_COMPILER \
     -B \
     -FEobj \
     -Tultibo \
     -O2 \
     -Parm \
     -CpARMV7A \
     -WpQEMUVPB \
     @/root/ultibo/core/fpc/bin/qemuvpb.cfg \
     $PROGRAM && \
     mv kernel.bin $OUTPUT
}

function intr {
    echo $* >> $QEMU_SCRIPT
}

function qemu {
    intr echo $*
}

SCREEN_NUMBER=1
function screendump {
    qemu screendump screen-$(printf "%02d" $SCREEN_NUMBER).ppm
    ((SCREEN_NUMBER+=1))
}

function make_qemu_script {
    touch $QEMU_SCRIPT
\
    intr sleep 2
    qemu -en \\\\001c
    screendump
    intr sleep 2
    screendump
    qemu quit
\
    chmod u+x $QEMU_SCRIPT
}

function run_qemu {
    echo running qemu ...
    ./$QEMU_SCRIPT | qemu-system-arm \
     -M versatilepb \
     -cpu cortex-a8 \
     -kernel kernel.bin \
     -m 256M \
     -display none \
     -serial mon:stdio |& tee raw.log | egrep -iv '^(alsa|pulseaudio:|audio:)' > serial.log
}
function unix_line_endings {
    tr -d \\r < $1 > tmp && \
    mv tmp $1
}

rm -rf $OUTPUT && \
mkdir -p $OUTPUT && \
build && \
\
cd $OUTPUT && \
\
make_qemu_script && \
run_qemu

if [ "$?" != "0" ]
then
    exit $?
fi

unix_line_endings raw.log
unix_line_endings serial.log
for screen in screen*.ppm
do
    convert $screen ${screen%.ppm}.png
    rm $screen
done
file *

grep -i error serial.log
if [ "$?" == "0" ]
then
    exit 1
fi
