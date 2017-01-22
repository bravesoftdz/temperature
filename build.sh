#!/bin/bash

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

function make-qemu-script {
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

function run-qemu {
    echo running qemu ...
    QEMU=$(ultibo-bash-quotation qemu-system-arm \
     -M versatilepb \
     -cpu cortex-a8 \
     -kernel kernel.bin \
     -m 256M \
     -display none \
     -serial mon:stdio)
    eval "./$QEMU_SCRIPT | $QEMU > raw.log 2>&1" && \
    cat raw.log | egrep -iv '^(alsa|pulseaudio:|audio:)' > serial.log
}

function unix_line_endings {
    tr -d \\r < $1 > tmp && \
    mv tmp $1
}

function test-qemu-target {
    local RESTORE_PWD=$(pwd)
    cd $1/$OUTPUT && \
    \
    make-qemu-script && \
    run-qemu
    if [[ $? -ne 0 ]]; then exit $?; fi

    unix_line_endings raw.log
    unix_line_endings serial.log
    sed -i 's/.\x1b.*\x1b\[D//' serial.log
    sed -i 's/\x1b\[K//' serial.log
    ls screen*.ppm > /dev/null 2>&1
    if [[ $? -eq 0 ]]
    then
        for screen in screen*.ppm
        do
            ultibo-bash convert $screen ${screen%.ppm}.png && \
            rm $screen
        done
    fi
    file *

    grep -i error serial.log
    if [[ $? -eq 0 ]]; then exit 1; fi

    cd $RESTORE_PWD
}

function build-target {
    echo .... building $4
    local INCLUDES=-Fi/root/ultibo/core/fpc/source/packages/fv/src
    rm -rf obj && \
    mkdir -p obj && \
    ultibo-bash fpc \
     -l- \
     -Sewn \
     -v0ewn \
     -B \
     -Tultibo \
     -O2 \
     -Parm \
     $2 \
     -Mdelphi \
     -FuSource \
     -FEobj \
     $INCLUDES \
     @/root/ultibo/core/fpc/bin/$3 \
     $4 |& tee build.log && \
\
    mv kernel* $1/$OUTPUT
#   if [[ $? -ne 0 ]]; then exit $?; fi
}

function build-QEMU {
    build-target $1 "-CpARMV7A -WpQEMUVPB" qemuvpb.cfg $2
    test-qemu-target $1
}

function build-RPi {
    build-target $1 "-CpARMV6 -WpRPIB" rpi.cfg $2
}

function build-RPi2 {
    build-target $1 "-CpARMV7A -WpRPI2B" rpi2.cfg $2
}

function build-RPi3 {
    build-target $1 "-CpARMV7A -WpRPI3B" rpi3.cfg $2
}

ULTIBO_BASE=$(pwd)
ARTIFACTS=$ULTIBO_BASE/$OUTPUT
rm -rf $ARTIFACTS
mkdir -p $ARTIFACTS

function build-as {
    local TARGET=$1
    local FOLDER=$2
    local REPO=$3
    if [[ -d $FOLDER ]]
    then
        ls $FOLDER/*.lpr > /dev/null 2>&1
        if [[ $? == 0 ]]
        then
            rm -rf $FOLDER/$OUTPUT
            mkdir -p $FOLDER/$OUTPUT
            build-$TARGET $FOLDER $FOLDER/*.lpr
            local THISOUT=$ARTIFACTS/$REPO/$FOLDER
            rm -rf $THISOUT
            mkdir -p $THISOUT
            cp -a $FOLDER/$OUTPUT/* $THISOUT
        fi
    fi
}

function build-as-2 {
    local TARGET=$1
    local FOLDER=$2
    local REPO=$3
    local PROGRAM=$4
    if [[ -d $FOLDER ]]
    then
            rm -rf $FOLDER/$OUTPUT
            mkdir -p $FOLDER/$OUTPUT
            build-$TARGET $FOLDER $PROGRAM
            local THISOUT=$ARTIFACTS/$REPO/$FOLDER/$TARGET
            rm -rf $THISOUT
            mkdir -p $THISOUT
            cp -a $FOLDER/$OUTPUT/* $THISOUT
    fi
}

function build-demo {
    cd $ULTIBO_BASE/gh/ultibohub/Demo
    for TARGET in RPi RPi2 RPi3
    do
        build-as-2 $TARGET . ultibohub/Demo "UltiboDemo$TARGET.lpr"
    done
}

function build-asphyre {
    cd $ULTIBO_BASE/gh/ultibohub/Asphyre
    local SAMPLES_PATH=Samples/FreePascal/Ultibo
    for SAMPLE_PATH in $SAMPLES_PATH/*
    do
        build-as RPi2 $SAMPLE_PATH ultibohub/Asphyre
    done
}

function build-example {
    TARGETS_PATH=$1
    if [[ -d $TARGETS_PATH ]]
    then
        for TARGET_PATH in $TARGETS_PATH/*
        do
            build-as $(basename $TARGET_PATH) $TARGET_PATH ultibohub/Examples
        done
    fi
}

function build-examples {
    cd $ULTIBO_BASE/gh/ultibohub/Examples
    for EXAMPLE in [0-9][0-9]-*
    do
        build-example $EXAMPLE
    done
    for EXAMPLE in Advanced/*
    do
        build-example $EXAMPLE
    done
}

build-demo
build-examples
build-asphyre
