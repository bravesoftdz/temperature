#!/bin/bash
BUILD_OUTPUT=$(pwd)/build-output
mkdir -p $BUILD_OUTPUT
LOG=$BUILD_OUTPUT/build.log
ERRORS=$BUILD_OUTPUT/build-errors.txt

function build {
    cd src
    cmd //c build_ultibo.bat
    cd ../examples/corelib/aggcanvas
    mkdir units
    cmd //c build_ultibo.bat
    cd ../canvastest
    cmd //c build_ultibo.bat
    cd ../helloworld
    cmd //c build_ultibo.bat
    cd ../..
}

function collect-errors {
    cat $LOG | egrep -i '(error|warning|note):' | sort | uniq > $ERRORS
    cat $ERRORS | tee -a $LOG
}

build |& tee $LOG

collect-errors
