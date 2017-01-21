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
    echo ......................... building $1/*.lpr
    local INCLUDES=-Fi/root/ultibo/core/fpc/source/packages/fv/src
    rm -rf obj && \
    mkdir -p obj && \
    ultibo-bash fpc \
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
     $1/*.lpr |& tee build.log && \
\
    mv kernel* $1/$OUTPUT
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
        rm -rf $OUTPUT
        mkdir -p $OUTPUT
        build-$TARGET $EXAMPLE/$TARGET
        local THISOUT=$ARTIFACTS/Examples/$EXAMPLE/$TARGET
        rm -rf $THISOUT
        mkdir -p $THISOUT
        cp -a $OUTPUT/* $THISOUT
        cd ..
    done
    cd ..
}

function build-examples {
    cd $ULTIBO_BASE/gh/ultibohub/Examples
    for EXAMPLE in [0-9][0-9]-*
    do
        build-example $EXAMPLE
    done

    for EXAMPLE in Advanced/*
    do
        if [ "$EXAMPLE" != "Advanced/README.md" ]
        then
            build-example $EXAMPLE
        fi
    done
}

ULTIBO_BASE=$(pwd)
ARTIFACTS=$ULTIBO_BASE/$OUTPUT
rm -rf $ARTIFACTS
mkdir -p $ARTIFACTS

function build-asphyre {
    cd $ULTIBO_BASE/gh/ultibohub/Asphyre
    local SAMPLES=Samples/FreePascal/Ultibo
    for SAMPLE in $SAMPLES/*
    do
        if [ "$SAMPLE" != "$SAMPLES/Media" ]
        then
            rm -rf $SAMPLE/$OUTPUT
            mkdir -p $SAMPLE/$OUTPUT
            build-RPi2 $SAMPLE
            local THISOUT=$ARTIFACTS/Asphyre/Samples/FreePascal/Ultibo/RPi2/$SAMPLE
            rm -rf $THISOUT
            mkdir -p $THISOUT
            cp -a $SAMPLE/$OUTPUT/* $THISOUT
        fi
    done
}

build-examples
build-asphyre
