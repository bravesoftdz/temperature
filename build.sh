#!/bin/bash

PROGRAM=pqemuframebuffer.lpr

OUTPUT=out
QEMU_SCRIPT=run-qemu.tmp

function ultibo-bash-quotation {
    if [ "$(which $1)" != "" ]
    then
        echo -n $*
    else
        local DOCKER_IMAGE=markfirmware/ultibo-bash
        echo -en "docker run --rm -i -v $(pwd):/workdir --entrypoint /bin/bash $DOCKER_IMAGE -c \"$*\""
    fi
}

function ultibo-bash {
    eval $(ultibo-bash-quotation $*)
}

function build {
    rm -rf obj && \
    mkdir -p obj && \
    ultibo-bash fpc \
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
    QEMU=$(ultibo-bash-quotation qemu-system-arm \
     -M versatilepb \
     -cpu cortex-a8 \
     -kernel kernel.bin \
     -m 256M \
     -display none \
     -serial mon:stdio)
    eval "./$QEMU_SCRIPT | $QEMU |& tee raw.log" && \
    cat raw.log | egrep -iv '^(alsa|pulseaudio:|audio:)' > serial.log
}

function unix_line_endings {
    tr -d \\r < $1 > tmp && \
    mv tmp $1
}

function main {
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
    sed -i 's/.\x1b.*\x1b\[D//' serial.log
    sed -i 's/\x1b\[K//' serial.log
    ls screen*.ppm
    if [ "$?" == "0" ]
    then
        for screen in screen*.ppm
        do
            ultibo-bash convert $screen ${screen%.ppm}.png && \
            ls -lt $screen
            rm $screen
        done
    fi
    file *

    grep -i error serial.log
    if [ "$?" == "0" ]
    then
        exit 1
    fi
}

function build-target {
    echo ......................... building $1 *.lpr
    local INCLUDES=-Fi/root/ultibo/core/fpc/source/packages/fv/src
    rm -rf obj && \
    mkdir -p obj && \
    ultibo-bash fpc \
     -B \
     -Tultibo \
     -O2 \
     -Parm \
     $2 \
     -FEobj \
     $INCLUDES \
     @/root/ultibo/core/fpc/bin/$3 \
     *.lpr |& tee build.log && \
\
    mv kernel* $OUTPUT
}

function build-QEMU {
    build-target $1 "-CpARMV7A -WpQEMUVPB" qemuvpb.cfg
}

function build-RPi {
    build-target $1 "-CpARMV6 -WpRPIB" rpi.cfg
}

function build-RPi2 {
    build-target $1 "-CpARMV7A -WpRPI2B" rpi2.cfg
}

function build-RPi3 {
    build-target $1 "-CpARMV7A -WpRPI3B" rpi3.cfg
}

function build-example {
    EXAMPLE="$1"
    cd $EXAMPLE
    echo
    echo $EXAMPLE
    for TARGET in *
    do
        cd $TARGET
        rm -rf $OUTPUT
        mkdir -p $OUTPUT
        build-$TARGET $EXAMPLE/$TARGET
        local THISOUT=$ARTIFACTS/$EXAMPLE/$TARGET
        rm -rf $THISOUT
        mkdir -p $THISOUT
        cp -a $OUTPUT/* $THISOUT
        cd ..
    done
    cd ..
}

function build-examples {
    for EXAMPLE in [0-9][0-9]-*
    do
        build-example $EXAMPLE
    done

    cd Advanced
    for EXAMPLE in *
    do
        if [ "$EXAMPLE" != "README.md" ]
        then
            build-example $EXAMPLE
        fi
    done
    cd ..
}

ARTIFACTS=$(pwd)/out
cd gh/ultibohub/Examples
build-examples
