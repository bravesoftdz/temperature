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

function make_qemu_script {
    touch $QEMU_SCRIPT && \
\
    echo sleep 2 >> $QEMU_SCRIPT && \
    echo echo -en \\\\001c >> $QEMU_SCRIPT && \
    echo echo screendump screen-01.ppm >> $QEMU_SCRIPT && \
    echo sleep 2 >> $QEMU_SCRIPT && \
    echo echo screendump screen-02.ppm >> $QEMU_SCRIPT && \
    echo echo quit >> $QEMU_SCRIPT && \
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
function convert_screen {
    convert $1.ppm $1.png && \
    rm $1.ppm
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
run_qemu && \
\
convert_screen screen-01 && \
convert_screen screen-02 && \
\
unix_line_endings raw.log && \
unix_line_endings serial.log && \
if [ "$?" != "0" ]
then
    exit $?
fi

file *
grep -i error serial.log
if [ "$?" == "0" ]
then
    exit 1
fi
